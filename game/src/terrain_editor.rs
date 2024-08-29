use core::f32;
use std::{cmp::Ordering, fmt::Display};

use bevy::{ecs::world::Command, prelude::*};
use engine::terrain::{Convert, PathGraph, Terrain, ToIsometry, M};
use leafwing_input_manager::{
    plugin::InputManagerPlugin,
    prelude::{ActionState, InputMap},
    Actionlike, InputManagerBundle,
};
use parry2d::math::{Point, Vector};
use sickle_ui::{
    prelude::{
        LabelConfig, SetAlignContentExt, SetAlignItemsExt, SetAlignSelfExt, SetBackgroundColorExt,
        SetJustifyContentExt, SetWidthExt, UiBuilderExt, UiColumnExt, UiLabelExt, UiRoot, UiRowExt,
    },
    widgets::layout::label::SetLabelTextExt,
};

use crate::game::MouseWorldPos;

pub struct TerrainEditorPlugin;

macro_rules! input_map {
    ($($btn:expr => $input:expr $(,)?)*) => {
        {
            let mut input_map = InputMap::default();

            $(input_map.insert($input, $btn);)*

            input_map
        }
    };
}

impl Plugin for TerrainEditorPlugin {
    fn build(&self, app: &mut App) {
        app.insert_state(EditorState::None);
        app.add_plugins(InputManagerPlugin::<Input>::default());
        app.add_plugins(EditorUiPlugin);
        app.add_plugins((
            EditorViewModePlugin,
            EditorAddModePlugin,
            EditorEditModePlugin,
        ));

        app.add_systems(Startup, add_input_listener);

        app.add_systems(Update, input_handler);

        app.add_systems(
            Update,
            draw_terrain.run_if(not(in_state(EditorState::None))),
        );
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, States)]
enum EditorState {
    None,
    View,
    Add,
    Edit,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Actionlike, Reflect)]
pub enum Input {
    ToggleEditor,

    EnterAddMode,
    EnterEditMode,

    Delete,
    Cancel,
    Confirm,
    Save,

    LeftClick,
}

fn just_pressed<I: Actionlike>(input: &I) -> impl FnMut(Res<ActionState<I>>) -> bool + '_ {
    |state: Res<'_, ActionState<I>>| state.just_pressed(input)
}

fn just_clicked(state: Res<ActionState<Input>>) -> bool {
    state.just_pressed(&Input::LeftClick)
}

fn add_input_listener(mut commands: Commands) {
    let input_map = input_map! {
        KeyCode::F3 => Input::ToggleEditor,

        KeyCode::KeyN => Input::EnterAddMode,
        KeyCode::KeyE => Input::EnterEditMode,

        KeyCode::Delete => Input::Delete,
        KeyCode::Escape => Input::Cancel,
        KeyCode::Enter => Input::Confirm,

        MouseButton::Left => Input::LeftClick,
    };

    commands.insert_resource(Tool::Select);

    commands.insert_resource(input_map);
    commands.insert_resource(ActionState::<Input>::default());
}

fn input_handler(
    input: Res<ActionState<Input>>,
    selected: Query<(), With<Selected>>,
    current_state: Res<State<EditorState>>,
    mut next_state: ResMut<NextState<EditorState>>,
) {
    if input.just_pressed(&Input::ToggleEditor) {
        if *current_state.get() == EditorState::None {
            next_state.set(EditorState::View);
        } else {
            next_state.set(EditorState::None);
        }
    }

    if input.just_pressed(&Input::EnterAddMode) && *current_state.get() == EditorState::View {
        next_state.set(EditorState::Add);
    }

    if input.just_pressed(&Input::EnterEditMode) && selected.iter().count() == 1 {
        next_state.set(EditorState::Edit);
    }
}

fn draw_terrain(terrain: Query<(&Terrain, &Transform, Option<&Selected>)>, mut gizmos: Gizmos) {
    for (terrain, trans, selected) in &terrain {
        for segment in terrain.nodes.segments() {
            let segment = segment.transformed(&trans.isometry());
            let a: Vec2 = segment.a.conv();
            let b: Vec2 = segment.b.conv();
            let color = if selected.is_some() {
                Color::srgb(1.0, 1.0, 1.0)
            } else {
                Color::BLACK
            };
            gizmos.line(a.extend(0.01), b.extend(0.01), color);
        }
    }
}

struct EditorViewModePlugin;

impl Plugin for EditorViewModePlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(InputManagerPlugin::<ViewModeInput>::default());
        let input_map = input_map! {
            KeyCode::KeyQ => ViewModeInput::SelectTool,
            KeyCode::KeyW => ViewModeInput::MoveTool,
            KeyCode::KeyM => ViewModeInput::Meshify,
        };
        app.insert_resource(input_map);
        app.insert_resource(ActionState::<ViewModeInput>::default());

        app.add_systems(
            Update,
            (
                change_tool,
                delete_selected.run_if(just_pressed(&Input::Delete)),
                select_terrain.run_if(resource_equals(Tool::Select).and_then(just_clicked)),
                move_terrain.run_if(resource_equals(Tool::Move)),
                meshify.run_if(just_pressed(&ViewModeInput::Meshify)),
            )
                .run_if(in_state(EditorState::View)),
        );
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Actionlike, Reflect)]
enum ViewModeInput {
    SelectTool,
    MoveTool,

    Meshify,
}

pub struct DeselectAll;

impl Command for DeselectAll {
    fn apply(self, world: &mut World) {
        let selected = world
            .query_filtered::<Entity, With<Selected>>()
            .iter(world)
            .collect::<Vec<_>>();
        for e in selected {
            world.entity_mut(e).remove::<Selected>();
        }
    }
}

#[derive(Debug, Clone, Copy, Component)]
struct Selected;

fn select_terrain(
    mouse_pos: Res<MouseWorldPos>,
    terrain: Query<(Entity, &Terrain, &Transform)>,
    mut commands: Commands,
) {
    if let Some(mouse_pos) = mouse_pos.0 {
        commands.add(DeselectAll);
        for (e, terrain, trans) in &terrain {
            let pos = trans.translation.xy() - mouse_pos;
            if parry2d::utils::point_in_poly2d(&pos.conv(), terrain.nodes.vertices()) {
                commands.entity(e).insert(Selected);
                break;
            }
        }
    }
}

fn delete_selected(selected: Query<Entity, With<Selected>>, mut commands: Commands) {
    for e in &selected {
        commands.entity(e).despawn_recursive();
    }
}

fn change_tool(input_state: Res<ActionState<ViewModeInput>>, mut active_tool: ResMut<Tool>) {
    for (input, tool) in [
        (ViewModeInput::MoveTool, Tool::Move),
        (ViewModeInput::SelectTool, Tool::Select),
    ] {
        if input_state.just_pressed(&input) {
            *active_tool = tool;
        }
    }
}

fn move_terrain(
    mut last_pos: Local<Option<Vec2>>,
    input: Res<ActionState<Input>>,
    mouse_pos: Res<MouseWorldPos>,
    mut terrain: Query<&mut Transform, With<Selected>>,
) {
    let is_holding = input.pressed(&Input::LeftClick);

    match (is_holding, &mut *last_pos, mouse_pos.0) {
        (true, None, Some(pos)) => {
            *last_pos = Some(pos);
        }
        (true, Some(last_pos), Some(mouse_pos)) => {
            let diff = mouse_pos - *last_pos;
            if diff.length() > 0.0 {
                for mut trans in &mut terrain {
                    trans.translation += diff.extend(0.0);
                }
                *last_pos = mouse_pos;
            }
        }
        (false, Some(_), _) => *last_pos = None,
        _ => {}
    }
}

fn meshify(
    terrain: Query<(Entity, &Terrain), With<Selected>>,
    asset_server: Res<AssetServer>,
    mut commands: Commands,
) {
    for (e, terrain) in &terrain {
        let vertices = terrain.nodes.vertices();
        let polyline = BoxedPolyline2d::new(vertices.iter().copied().map(Convert::conv));
        let extrusion = Extrusion::new(M(polyline), 2.0);
        let mesh = extrusion.mesh().build();
        let handle = asset_server.add(mesh);
        let material = asset_server.add(StandardMaterial::from_color(Color::srgb(1.0, 0.7, 0.4)));
        commands.entity(e).insert((handle, material));
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Resource)]
enum Tool {
    Select,
    Move,
}

impl Display for Tool {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Tool::Select => "Select",
            Tool::Move => "Move",
        })
    }
}

pub struct EditorAddModePlugin;

impl Plugin for EditorAddModePlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(OnEnter(EditorState::Add), |mut commands: Commands| {
            commands.add(DeselectAll);
            commands.insert_resource(ActiveObject(vec![]));
        })
        .add_systems(
            Update,
            (
                create_or_add_to_object_on_click.run_if(just_clicked),
                finalize_object_on_confirm.run_if(just_pressed(&Input::Confirm)),
                exit_state.run_if(just_pressed(&Input::Cancel)),
                draw_active_object,
            )
                .run_if(in_state(EditorState::Add)),
        );
    }
}

#[derive(Resource)]
struct ActiveObject(Vec<Vec2>);

fn create_or_add_to_object_on_click(
    mut object: ResMut<ActiveObject>,
    mouse_pos: Res<MouseWorldPos>,
) {
    if let Some(mouse_pos) = mouse_pos.0 {
        object.0.push(mouse_pos);
    }
}

fn draw_active_object(object: Res<ActiveObject>, mut gizmos: Gizmos) {
    let color = Color::srgb(0.3, 0.3, 1.0);
    if let [pos] = object.0.as_slice() {
        gizmos.sphere(pos.extend(0.01), Quat::IDENTITY, 0.1, color);
    }

    for [a, b] in object.0.array_windows::<2>() {
        gizmos.line(a.extend(0.01), b.extend(0.01), color);
    }

    if object.0.len() > 2 {
        gizmos.line(
            object.0[0].extend(0.01),
            object.0[object.0.len() - 1].extend(0.01),
            color.with_alpha(0.5),
        );
    }
}

fn finalize_object_on_confirm(
    mut object: ResMut<ActiveObject>,
    mut next_state: ResMut<NextState<EditorState>>,
    mut commands: Commands,
) {
    fn is_ccw(vertices: &[Vec2]) -> bool {
        let (a_index, a) =
            vertices
                .iter()
                .enumerate()
                .fold((0, Vec2::new(0.0, f32::INFINITY)), |c, n| {
                    match n.1.y.partial_cmp(&c.1.y) {
                        Some(Ordering::Equal) => {
                            if n.1.x > c.1.x {
                                (n.0, *n.1)
                            } else {
                                c
                            }
                        }
                        Some(Ordering::Less) => (n.0, *n.1),
                        Some(Ordering::Greater) => c,
                        None => c,
                    }
                });

        let b_index = if a_index == 0 {
            vertices.len() - 1
        } else {
            a_index - 1
        };
        let c_index = (a_index + 1) % vertices.len();

        let b = vertices[b_index];
        let c = vertices[c_index];

        let ab = b - a;
        let ac = c - a;
        let cross = ab.perp_dot(ac);

        cross.is_sign_negative()
    }

    let mut vertices = std::mem::take(&mut object.0);
    if !is_ccw(&vertices) {
        vertices.reverse();
    }

    let mut center = Vec2::ZERO;
    for vert in &vertices {
        center += *vert;
    }
    center /= vertices.len() as f32;
    for vert in &mut vertices {
        *vert -= center;
    }
    let terrain = Terrain::from_vertices(vertices);
    commands.spawn((
        SpatialBundle::from_transform(Transform::from_translation(center.extend(0.0))),
        terrain,
    ));
    next_state.set(EditorState::View);
}

fn exit_state(mut next_state: ResMut<NextState<EditorState>>) {
    next_state.set(EditorState::View);
}

struct EditorEditModePlugin;

impl Plugin for EditorEditModePlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(
            Update,
            (
                drag_nodes,
                exit_state
                    .run_if(just_pressed(&Input::Confirm).or_else(just_pressed(&Input::Cancel))),
            )
                .run_if(in_state(EditorState::Edit)),
        );
    }
}

fn drag_nodes(
    mut dragging: Local<Option<(usize, Vec2)>>,
    input: Res<ActionState<Input>>,
    mouse_pos: Res<MouseWorldPos>,
    mut terrain: Query<(&mut Terrain, &Transform), With<Selected>>,
) {
    let holding = input.pressed(&Input::LeftClick);

    let (mut terrain, transform) = terrain.single_mut();

    fn find_closest_vertex(terrain: &Terrain, local_pos: Vec2) -> usize {
        let local_pos: Point<f32> = local_pos.conv();
        terrain
            .nodes
            .vertices()
            .iter()
            .enumerate()
            .fold((0, f32::INFINITY), |a, b| {
                let mag = (b.1 - local_pos).magnitude_squared();
                if mag < a.1 {
                    (b.0, mag)
                } else {
                    a
                }
            })
            .0
    }

    match (holding, &mut *dragging, mouse_pos.0) {
        (true, None, Some(mouse_pos)) => {
            let local_pos = mouse_pos - transform.translation.xy();
            let vertex = find_closest_vertex(&terrain, local_pos);
            *dragging = Some((vertex, mouse_pos));
        }
        (true, Some(last_pos), Some(mouse_pos)) => {
            let diff = mouse_pos - last_pos.1;
            let mut verts = terrain.nodes.vertices().to_vec();
            let diff: Vector<f32> = diff.conv();
            verts[last_pos.0] += diff;
            *terrain = Terrain::from_points(verts);
            last_pos.1 = mouse_pos;
        }
        (false, Some(_), _) => {
            *dragging = None;
        }
        _ => {}
    }
}

// UI

struct EditorUiPlugin;

impl Plugin for EditorUiPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(OnEnter(EditorState::View), setup_view_ui)
            .add_systems(OnExit(EditorState::View), destroy_ui)
            .add_systems(OnEnter(EditorState::Add), setup_add_ui)
            .add_systems(OnExit(EditorState::Add), destroy_ui)
            .add_systems(OnEnter(EditorState::Edit), setup_edit_ui)
            .add_systems(OnExit(EditorState::Edit), destroy_ui)
            .add_systems(
                Update,
                (update_tool_display
                    .run_if(resource_changed::<Tool>)
                    .run_if(in_state(EditorState::View))),
            );
    }
}

#[derive(Component)]
struct ToolLabel;

fn setup_view_ui(tool: Res<Tool>, mut commands: Commands) {
    commands
        .ui_builder(UiRoot)
        .column(|builder| {
            builder
                .label(LabelConfig::from("View Mode"))
                .style()
                .align_self(AlignSelf::Start);
            builder.row(|builder| {
                builder.label(LabelConfig::from("Active Tool: "));
                builder
                    .label(LabelConfig::from(tool.to_string()))
                    .insert(ToolLabel);
            });
            builder
                .label(LabelConfig::from("Q - Select Tool"))
                .style()
                .align_self(AlignSelf::Start);
            builder
                .label(LabelConfig::from("W - Move Tool"))
                .style()
                .align_self(AlignSelf::Start);
            builder
                .label(LabelConfig::from("N - Add New Terrain"))
                .style()
                .align_self(AlignSelf::Start);
            builder
                .label(LabelConfig::from("E - Edit Selected Terrain"))
                .style()
                .align_self(AlignSelf::Start);
            builder
                .label(LabelConfig::from("Delete - Delete Selected Terrain"))
                .style()
                .align_self(AlignSelf::Start);
        })
        .style()
        .align_items(AlignItems::Start);
}

fn update_tool_display(
    tool: Res<Tool>,
    label: Query<Entity, With<ToolLabel>>,
    mut commands: Commands,
) {
    let label = label.single();
    commands.entity(label).set_label_text(tool.to_string());
}

fn setup_add_ui(mut commands: Commands) {
    commands.ui_builder(UiRoot).column(|builder| {
        builder
            .label(LabelConfig::from("Add Mode"))
            .style()
            .align_self(AlignSelf::Start);
        builder
            .label(LabelConfig::from("Click to add terrain vertices"))
            .style()
            .align_self(AlignSelf::Start);
        builder
            .label(LabelConfig::from("Enter - Confirm terrain object"))
            .style()
            .align_self(AlignSelf::Start);
        builder
            .label(LabelConfig::from("Escape - Cancel terrain object"))
            .style()
            .align_self(AlignSelf::Start);
    });
}

fn setup_edit_ui(mut commands: Commands) {
    commands.ui_builder(UiRoot).column(|builder| {
        builder
            .label(LabelConfig::from("Edit Mode"))
            .style()
            .align_self(AlignSelf::Start);
        builder
            .label(LabelConfig::from("Click and drag to drag terrain vertices"))
            .style()
            .align_self(AlignSelf::Start);
        builder
            .label(LabelConfig::from("Enter/Escape - Exit edit mode"))
            .style()
            .align_self(AlignSelf::Start);
    });
}

fn destroy_ui(root_nodes: Query<Entity, (With<Node>, Without<Parent>)>, mut commands: Commands) {
    for node in &root_nodes {
        commands.entity(node).despawn_recursive();
    }
}
