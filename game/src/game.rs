use std::time::SystemTime;

use bevy::{prelude::*, window::PrimaryWindow};
use engine::{actor::unit::UnitController, CameraFocus, CoreGameplay, GameRequest, GameServerConn, RenderDebug};
use leafwing_input_manager::{
    plugin::InputManagerPlugin,
    prelude::{ActionState, InputMap, MouseScroll, MouseScrollAxis, MouseScrollDirection},
    Actionlike, InputControlKind, InputManagerBundle,
};

use crate::{lobby::MyPlayerId, TopLevelState};

pub struct ClientGamePlugin;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SystemSet)]
pub struct InputHandling;

impl Plugin for ClientGamePlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(InputManagerPlugin::<Input>::default());

        app.insert_resource(MouseWorldPos(None));

        app.configure_sets(
            Update,
            (InputHandling.before(CoreGameplay)).run_if(in_state(TopLevelState::InGame)),
        );

        app.add_systems(Startup, setup_unit_control);
        app.add_systems(
            Update,
            mouse_tracking.run_if(in_state(TopLevelState::InGame)),
        );
        app.add_systems(
            Update,
            (mouse_tracking, handle_camera_control, handle_unit_control)
                .chain()
                .in_set(InputHandling),
        );
    }
}

fn setup_unit_control(mut commands: Commands) {
    commands.observe(
        |trigger: Trigger<OnAdd, UnitController>,
         my_id: Res<MyPlayerId>,
         q: Query<&UnitController>,
         mut commands: Commands| {
            let e = trigger.entity();
            match q.get(e).unwrap() {
                UnitController::Player(id) if my_id.0 == *id => {
                    let mut input_map = InputMap::new([
                        (Input::LeftClick, MouseButton::Left),
                        (Input::RightClick, MouseButton::Right),
                        (Input::DragCamera, MouseButton::Middle),
                    ]);
                    input_map.insert(Input::FocusCamera, KeyCode::Space);
                    input_map.insert(Input::ResetCameraZoom, KeyCode::Digit0);
                    input_map.insert(Input::ToggleRenderDebug, KeyCode::F1);
                    input_map.insert_axis(Input::ZoomCamera, MouseScrollAxis::Y);
                    commands
                        .entity(e)
                        .insert(InputManagerBundle::with_map(input_map));
                }
                _ => {}
            }
        },
    );

    commands.observe(
        |trigger: Trigger<OnRemove, UnitController>,
         my_id: Res<MyPlayerId>,
         q: Query<&UnitController>,
         mut commands: Commands| {
            let e = trigger.entity();
            match q.get(e).unwrap() {
                UnitController::Player(id) if my_id.0 == *id => {
                    commands.entity(e).remove::<InputManagerBundle<Input>>();
                }
                _ => {}
            }
        },
    );
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug, Reflect)]
enum Input {
    LeftClick,
    RightClick,
    FocusCamera,
    DragCamera,
    ZoomCamera,
    ResetCameraZoom,

    ToggleRenderDebug,
}

impl Actionlike for Input {
    fn input_control_kind(&self) -> leafwing_input_manager::InputControlKind {
        match self {
            Self::ZoomCamera => InputControlKind::Axis,
            _ => InputControlKind::Button,
        }
    }
}

#[derive(Resource)]
pub struct MouseWorldPos(pub Option<Vec2>);

fn mouse_tracking(
    mut pos: ResMut<MouseWorldPos>,
    q_windows: Query<&Window, With<PrimaryWindow>>,
    camera: Query<(&GlobalTransform, &Projection, &Camera)>,
) {
    let (trans, proj, _) = camera
        .iter()
        .filter_map(|(a, b, c)| match b {
            Projection::Perspective(proj) => Some((a, proj, c)),
            Projection::Orthographic(_) => None,
        })
        .min_by_key(|x| x.2.order)
        .unwrap();

    let window = q_windows.single();
    let cursor_pos = window.cursor_position().map(|pos| pos / window.size());

    let cursor_pos = if cursor_pos.is_none() {
        if pos.0.is_some() {
            pos.0 = None;
        }
        return;
    } else {
        cursor_pos.unwrap()
    };

    let p = trans.translation();

    let m = (cursor_pos - 0.5) * Vec2::new(1.0, -1.0);
    let v_fov = proj.fov;

    // u = v / 2
    // tan(u) = h / 1.0 = h

    let h = (v_fov / 2.0).tan() * 2.0;
    let w = h * proj.aspect_ratio;

    let x = m.x * w;
    let y = m.y * h;

    let d = trans.forward().as_vec3() + trans.right() * x + trans.up() * y;
    let d = d.normalize();

    // p.z + d.z * t = 0
    // d.z * t = -p.z
    // t = -p.z / d.z
    let t = -p.z / d.z;
    let p = p + d * t;

    pos.0 = Some(p.xy());
}

fn handle_camera_control(
    mut drag_pos: Local<Option<Vec2>>,
    mouse_pos: Res<MouseWorldPos>,
    input: Query<&ActionState<Input>>,
    mut tracker: Query<&mut Transform, With<CameraFocus>>,
    mut camera_pos: Query<&mut Transform, (With<Camera>, Without<CameraFocus>)>,
    time: Res<Time>,
) {
    let Ok(input) = input.get_single() else {
        return;
    };
    let Some(mouse_pos) = mouse_pos.0 else {
        return;
    };

    if input.pressed(&Input::DragCamera) {
        match *drag_pos {
            Some(pos) => {
                let diff = mouse_pos - pos;
                tracker.single_mut().translation -= diff.extend(0.0);
            }
            None => {
                *drag_pos = Some(mouse_pos);
            }
        }
    } else if input.just_released(&Input::DragCamera) {
        *drag_pos = None;
    }

    if let Some(axis) = input.axis_data(&Input::ZoomCamera) {
        let zoom = axis.value;
        let mut trans = camera_pos.single_mut();
        let shift = trans.forward() * zoom * time.delta_seconds() * trans.translation.length();
        trans.translation += shift;
    }

    if input.pressed(&Input::ResetCameraZoom) {
        let mut trans = camera_pos.single_mut();
        trans.translation = engine::DEFAULT_CAM_POS;
    }
}

fn handle_unit_control(
    q: Query<(&ActionState<Input>, &Transform)>,
    mouse_pos: Res<MouseWorldPos>,
    mut camera_tracker: Query<&mut Transform, (With<CameraFocus>, Without<ActionState<Input>>)>,
    mut conn: ResMut<GameServerConn>,
    mut render_debug: ResMut<RenderDebug>,
) {
    for (input, player_trans) in &q {
        if input.just_pressed(&Input::RightClick)
            && let Some(mouse_pos) = mouse_pos.0
        {
            // println!("MOVING");

            // println!(
            //     "\nCLICK - {}",
            //     SystemTime::now()
            //         .duration_since(SystemTime::UNIX_EPOCH)
            //         .unwrap()
            //         .as_secs_f64()
            // );
            conn.0
                .write(&GameRequest::SetMovementTarget(mouse_pos))
                .unwrap();
        }

        if input.pressed(&Input::FocusCamera) {
            camera_tracker.single_mut().translation = player_trans.translation;
        }

        if input.just_pressed(&Input::ToggleRenderDebug) {
            render_debug.0 = !render_debug.0;
        }
    }
}
