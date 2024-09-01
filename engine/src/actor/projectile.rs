use bevy::prelude::*;

use crate::Mode;

use super::{ActorId, ActorMap};

pub struct ProjectilePlugin(pub Mode);

impl Plugin for ProjectilePlugin {
    fn build(&self, app: &mut App) {
        let schedule = self.0.schedule();
        app.add_systems(schedule, projectile);
    }
}

#[derive(Component)]
pub struct Projectile;

#[derive(Component)]
pub enum ProjectileTarget {
    Actor(ActorId),
    Skillshot(Vec2)
}

#[derive(Component)]
pub struct ProjectileSpeed(pub f32);

#[derive(Event)]
pub struct ProjectileHit(pub ActorId);

fn projectile(mut q: Query<(Entity, &mut Transform, &ProjectileTarget, &ProjectileSpeed), With<Projectile>>, t: Query<&Transform, Without<Projectile>>, actor_map: Res<ActorMap>, time: Res<Time>, mut commands: Commands) {
    for (ent, mut trans, target, speed) in &mut q {
        match target {
            ProjectileTarget::Actor(target) => {
                let target_e = actor_map.0.get(target).unwrap();
                let target_trans = t.get(*target_e).unwrap();
                let diff = target_trans.translation.xy() - trans.translation.xy();
                if diff.length() < speed.0 * time.delta_seconds() {
                    // HIT!
                    commands.trigger_targets(ProjectileHit(*target), ent);
                } else {
                    let dir = diff.normalize();
                    trans.translation += dir.extend(0.0) * speed.0 * time.delta_seconds();
                }
            },
            ProjectileTarget::Skillshot(dir) => {
                trans.translation += dir.extend(0.0) * speed.0 * time.delta_seconds();
            },
        }
    }
}
