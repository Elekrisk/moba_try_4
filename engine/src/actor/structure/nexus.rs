use bevy::prelude::*;
use uuid::Uuid;

use crate::{actor::ActorId, actor_id, terrain::Terrain, ActorBuilder, ActorSpawner};

actor_id!(ID = "e38da48c-3f5f-4532-b477-cf9e9054f4e9");

pub struct NexusSpawner;
pub struct NexusData;
pub struct NexusBuilder;

fn bundle() -> impl Bundle {
    Terrain::circle(2.0, 8)
}

impl ActorSpawner for NexusSpawner {
    type Data = NexusData;
    type SendData = ();

    fn spawn(&self, pos: Vec2, data: Self::Data, commands: &mut Commands) -> crate::ActorSpawned {
        let id = ActorId(Uuid::new_v4());
        commands.spawn((
            TransformBundle::from_transform(Transform::from_translation(pos.extend(0.0))),
            id,
            bundle(),
        ));

        crate::ActorSpawned {
            actor_id: id,
            pos,
            actor_type: crate::ActorType::Other(crate::OtherData::Bytes(vec![])),
            kind_id: ID,
            data: vec![],
        }
    }
}

impl ActorBuilder for NexusBuilder {
    fn actor_kind_id(&self) -> crate::ActorKindId {
        ID
    }

    fn build(&self, data: crate::ActorSpawned, asset_server: &AssetServer, commands: &mut Commands) {
        let pos = data.pos;
        println!("Nexus @ {pos}");
        commands.spawn((
            SceneBundle {
                scene: asset_server.load(GltfAssetLabel::Scene(0).from_asset("nexus.glb")),
                transform: Transform::from_translation(pos.extend(0.0)).looking_to(Vec3::Y, Vec3::Z),
                ..default()
            },
            data.actor_id,
            bundle(),
        ));
    }
}
