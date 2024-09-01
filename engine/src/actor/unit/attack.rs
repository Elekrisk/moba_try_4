use bevy::prelude::*;

use crate::{actor::MovementTarget, Mode};

pub struct AttackPlugin(pub Mode);

impl Plugin for AttackPlugin {
    fn build(&self, app: &mut App) {
        let schedule = self.0.schedule();

        app.add_systems(schedule, (walk_into_range_system, dec_attack_timer));
    }
}

#[derive(Component)]
pub struct AttackTarget(pub Option<Entity>);

#[derive(Component)]
pub struct AttackTimer(pub f32);

#[derive(Event)]
pub struct AttackEvent(pub Entity);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Component)]
pub enum AttackType {
    Melee,
    Ranged
}

fn walk_into_range_system(mut query: Query<(Entity, &Transform, &AttackTarget, &mut MovementTarget, &AttackType, &mut AttackTimer)>, trans_q: Query<&Transform>, mut commands: Commands) {
    let attack_range = 2.0;

    for (e, trans, attack, mut movement, attack_type, mut attack_timer) in &mut query {
        if attack.0.is_none() {
            continue;
        }

        let target = attack.0.unwrap();
        let target_trans = trans_q.get(target).unwrap();

        let diff = (target_trans.translation.xy() - trans.translation.xy()).length_squared();

        if diff <= attack_range * attack_range {
            movement.0 = None;
            
            if attack_timer.0 <= 0.0 {
                commands.trigger_targets(AttackEvent(target), e);
                attack_timer.0 = 1.0;
            }
            // match attack_type {
            //     AttackType::Melee => {
            //     },
            //     AttackType::Ranged => todo!(),
            // }
        } else {
            movement.0 = Some(target_trans.translation.xy());
        }
    }
}

fn dec_attack_timer(mut q: Query<&mut AttackTimer>, time: Res<Time>) {
    for mut timer in &mut q {
        timer.0 -= time.delta_seconds();
    }
}
