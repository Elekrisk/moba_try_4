use bevy::prelude::*;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

pub mod unit;

#[derive(Component, Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ActorId(pub Uuid);

#[derive(Bundle)]
pub struct ActorBundle {
    mesh_bundle: MaterialMeshBundle<StandardMaterial>,
    actor: ActorId,
}
