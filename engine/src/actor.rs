use core::f32;
use std::{f32::consts::PI, time::{Duration, SystemTime}};

use bevy::{
    ecs::{component::Tick, system::SystemChangeTick},
    prelude::*,
};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::{anim::{AnimationController, AnimationInfo, AnimationKind}, terrain::PathGraph};

pub mod unit;
pub mod structure;

#[derive(Default, Component, Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ActorId(pub Uuid);

#[derive(Default, Bundle)]
pub struct ActorBundle {
    actor: ActorId,
    target: MovementTarget,
    path: MovementPath,
}

#[derive(Default, Bundle)]
pub struct ServerActorBundle {
    transform: TransformBundle,
    actor: ActorId,
    target: MovementTarget,
    path: MovementPath,
}

#[derive(Default, Component)]
pub struct MovementTarget(pub Option<Vec2>);

#[derive(Default, Component)]
pub struct MovementPath(pub Vec<Vec2>);

pub fn debug_movement_path(q: Query<(&Transform, &MovementPath)>, mut gizmos: Gizmos) {
    for (trans, path) in &q {
        let mut cur = trans.translation.xy();

        for pos in &path.0 {
            gizmos.line(cur.extend(0.11), pos.extend(0.11), Color::srgb(0.0, 1.0, 0.0));
            cur = *pos;
        }
    }
}

pub fn move_to_target(
    mut q: Query<(&mut MovementTarget, &mut MovementPath, &mut Transform, Option<(&AnimationInfo, &mut AnimationController)>)>,
    terrain: Res<PathGraph>,
    time: Res<Time>,
) {
    for (mut target, mut path, mut trans, anim) in &mut q {
        if let Some(pos) = target.0 {

            if let Some((i, mut c)) = anim {
                if Some(c.current_animation()) != i.animations.get(&AnimationKind::Moving).copied() {
                    c.play(*i.animations.get(&AnimationKind::Moving).unwrap());
                    println!("Swapping animation");
                }
            }

            let needs_new_path = target.is_changed() || path.0.last() != Some(&pos)
                || terrain.last_changed().get() > path.last_changed().get();
            if needs_new_path {
                let new_path = terrain.get_path(trans.translation.xy(), pos);
                // println!("{new_path:?}");
                path.0 = new_path;
            }

            let delta = time.delta_seconds();
            let speed = 2.0;
            let rot_speed = PI * 5.0;
            let adjusted_speed = speed * delta;
            let adjusted_rot_speed = rot_speed * delta;
            let mut remaining_travel = adjusted_speed;
            let mut cur_pos = trans.translation.xy();

            while remaining_travel > 0.0 {
                let Some(&next_point) = path.0.first() else {
                    break;
                };
                let diff = next_point - cur_pos;
                let dir = diff.normalize();

                if dir.length_squared() > f32::EPSILON {
                    let dir_diff = dir.angle_between(trans.forward().xy());
                    let mut dir_diff = dir_diff.clamp(-adjusted_rot_speed, adjusted_rot_speed);
                    if dir_diff == 0.0 && trans.forward().xy() == -dir {
                        dir_diff = adjusted_rot_speed;
                    }
                    // println!("{dir_diff} {} {}", trans.forward().xy(), dir);

                    let dir = Vec2::from_angle(-dir_diff).rotate(trans.forward().xy());

                    trans.look_to(dir.extend(0.0), Vec3::Z);
                }

                if diff.length_squared() <= remaining_travel * remaining_travel {
                    cur_pos = next_point;
                    remaining_travel -= diff.length();
                    path.0.remove(0);
                } else {
                    cur_pos += dir * remaining_travel;
                    break;
                }
            }

            if path.0.is_empty() {
                target.0 = None;
            }

            trans.translation = cur_pos.extend(trans.translation.z);
        } else {
            if let Some((i, mut c)) = anim {
                if Some(c.current_animation()) != i.animations.get(&AnimationKind::Idle).copied() {
                    c.play(*i.animations.get(&AnimationKind::Idle).unwrap());
                }
            }
        }
    }
}
