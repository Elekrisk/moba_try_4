use bevy::app::Plugin;
use bevy::prelude::*;
use bevy::transform::commands;
use bevy::utils::HashMap;
use bevy::window::PrimaryWindow;
use sickle_ui::prelude::*;
use sickle_ui::widgets::layout::label::SetLabelTextExt;

#[derive(Debug, Clone, Copy, Component)]
pub struct Health {
    pub health: f32,
}

pub struct HealthBarPlugin;

impl Plugin for HealthBarPlugin {
    fn build(&self, app: &mut App) {
        app.observe(create_hp_bar);
        app.add_systems(Update, (update_hp_bar, (update_hp_bar_pos, apply_deferred).chain()));
        app.init_resource::<HpBarMap>();
    }
}

#[derive(Default, Resource)]
struct HpBarMap(HashMap<Entity, Entity>);

#[derive(Debug, Component)]
struct HpBar {
    tracking_entity: Entity,
    label_id: Entity,
}

fn create_hp_bar(
    trigger: Trigger<OnAdd, Health>,
    mut hp_map: ResMut<HpBarMap>,
    mut commands: Commands,
) {
    let e = trigger.entity();

    let mut label_id = Entity::PLACEHOLDER;

    commands
        .ui_builder(UiRoot)
        .column(|builder| {
            let id = builder.label(LabelConfig::from("%HEALTH%")).id();
            label_id = id;
            hp_map.0.insert(e, id);
        })
        .insert(HpBar {
            tracking_entity: e,
            label_id,
        })
        .style()
        .width(Val::Px(100.0))
        .height(Val::Px(25.0));
}

fn update_hp_bar(
    hp: Query<(Entity, &Health), Changed<Health>>,
    window: Query<&Window, With<PrimaryWindow>>,
    camera: Query<(&GlobalTransform, &Projection)>,
    hp_map: Res<HpBarMap>,
    mut commands: Commands,
) {
    for (e, hp) in &hp {
        let id = hp_map.0.get(&e).unwrap();
        commands
            .entity(*id)
            .set_label_text(format!("{:.0}", hp.health));
    }
}

fn update_hp_bar_pos(
    hp_bar: Query<(Entity, &HpBar, &Style)>,
    hp: Query<&Transform, With<Health>>,
    camera: Query<(&Camera, &GlobalTransform)>,
    hp_map: Res<HpBarMap>,
    mut commands: Commands,
) {
    for (e, hp_bar, style) in &hp_bar {
        let trans = hp.get(hp_bar.tracking_entity).unwrap();
        let (cam, cam_trans) = camera.single();

        let Some(pos) =
            cam.world_to_viewport(cam_trans, trans.translation + Vec3::Z * 2.0 + Vec3::Y * 0.5)
        else {
            println!("Failed to compute screen coords");
            continue;
        };

        let Val::Px(w) = style.width else { panic!() };
        let Val::Px(h) = style.height else { panic!() };

        commands
            .style(e)
            .absolute_position(pos - Vec2::new(w / 2.0, h));
    }
}
