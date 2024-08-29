use bevy::prelude::*;
use lobby_server::PlayerId;
use serde::{Deserialize, Serialize};

use super::{ActorBundle, ServerActorBundle};

pub mod champion;

#[derive(Default, Debug, Clone, Copy, Component)]
pub struct Unit;

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
