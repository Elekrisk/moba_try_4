use bevy::prelude::*;
use lobby_server::PlayerId;
use serde::{Deserialize, Serialize};

use super::ActorBundle;

pub mod champion;

#[derive(Debug, Clone, Copy, Component)]
pub struct Unit;

#[derive(Bundle)]
pub struct UnitBundle {
    actor: ActorBundle,
    unit: Unit,
    controller: UnitController,
}

#[derive(Component)]
pub enum UnitController {
    Player(PlayerId),
    Ai(!),
}

#[derive(Debug, Serialize, Deserialize)]
pub enum UnitControllerInfo {
    Player(PlayerId),
}
