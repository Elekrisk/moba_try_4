use std::f32::consts::PI;

use bevy::prelude::*;
use uuid::Uuid;

use crate::{
    actor::ActorId, actor_id, terrain::Terrain, ActorBuilder, ActorKindId, ActorSpawned, ActorSpawner, ActorType, OtherData
};

actor_id!(ID = "55317883-c37f-4656-ab76-8ca0bb63a911");

pub struct TowerSpawner;

impl TowerSpawner {
    fn bundle() -> impl Bundle {
        Terrain::circle(1.0, 8)
    }
}

pub struct TowerData;

impl ActorSpawner for TowerSpawner {
    type Data = TowerData;
    type SendData = ();

    fn spawn(
        &self,
        pos: Vec2,
        data: Self::Data,
        commands: &mut bevy::prelude::Commands,
    ) -> crate::ActorSpawned {
        let id = ActorId(Uuid::new_v4());

        commands.spawn((
            TransformBundle::from_transform(Transform::from_translation(pos.extend(0.0))),
            id,
            Self::bundle()
        ));

        ActorSpawned {
            actor_id: id,
            pos,
            actor_type: ActorType::Other(OtherData::Bytes(vec![])),
            kind_id: ID,
            data: vec![],
        }
    }
}

pub struct TowerBuilder;

impl ActorBuilder for TowerBuilder {
    fn actor_kind_id(&self) -> ActorKindId {
        ID
    }

    fn build(
        &self,
        data: ActorSpawned,
        asset_server: &bevy::prelude::AssetServer,
        commands: &mut bevy::prelude::Commands,
    ) {
        let pos = data.pos;

        let mesh = Cylinder::new(1.0, 2.0).mesh().build();
        let mesh = asset_server.add(mesh);
        let material = StandardMaterial::from_color(Color::srgb(0.0, 1.0, 0.0));
        let material = asset_server.add(material);

        commands.spawn((
            MaterialMeshBundle {
                mesh,
                material,
                transform: Transform::from_translation(pos.extend(1.0)).looking_to(Vec3::Y, Vec3::Z),
                ..default()
            },
            data.actor_id,
            TowerSpawner::bundle()
        ));
    }
}
