use attack::{AttackPlugin, AttackTarget, AttackTimer};
use bevy::prelude::*;
use lobby_server::{PlayerId, Team};
use serde::{Deserialize, Serialize};

use crate::Mode;

use super::{actor_bundle, ActorBundle, ActorId, ServerActorBundle};

pub mod champion;
pub mod attack;

pub struct UnitPlugin(pub Mode);

impl Plugin for UnitPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(AttackPlugin(self.0));
    }
}

#[derive(Default, Debug, Clone, Copy, Component)]
pub struct Unit;

#[derive(Debug, Clone, Copy, Component)]
pub struct OnTeam(pub Team);

pub fn unit_bundle(actor_id: ActorId, transform: Transform, controller: UnitController, team: Team) -> impl Bundle {
    (
        actor_bundle(actor_id, transform),
        controller,
        Unit,
        OnTeam(team),
        AttackTarget(None),
        AttackTimer(0.0),
    )
}

#[derive(Default, Bundle)]
pub struct UnitBundle {
    actor: ActorBundle,
    unit: Unit,
    controller: UnitController,
}

#[derive(Default, Bundle)]
pub struct ServerUnitBundle {
    actor: ServerActorBundle,
    unit: Unit,
    controller: UnitController,
}

#[derive(Default, Component)]
pub enum UnitController {
    #[default]
    None,
    Player(PlayerId),
    Ai(!),
}

#[derive(Debug, Serialize, Deserialize)]
pub enum UnitControllerInfo {
    Player(PlayerId),
}
