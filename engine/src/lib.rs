#![feature(never_type)]
#![feature(anonymous_lifetime_in_impl_trait)]
#![feature(decl_macro)]
#![feature(get_many_mut)]

pub mod actor;
pub mod anim;
pub mod terrain;

use core::f32;
use std::{any::Any, sync::mpsc, time::SystemTime};

use actor::{
    structure::{
        nexus::{NexusBuilder, NexusData, NexusSpawner},
        tower::{TowerBuilder, TowerData, TowerSpawner},
    },
    unit::{
        champion::champ_1::{Champ1Builder, Champ1Data, Champ1Spawner},
        UnitController, UnitControllerInfo,
    },
    ActorId, MovementTarget,
};
use bevy::{
    asset::AssetEvents, ecs::system::RunSystemOnce, gizmos::gizmos::GizmoStorage, prelude::*,
    utils::HashMap,
};
use lobby_server::{Connection, PlayerId, Team};
use parry2d::{
    math::{Isometry, Point, Vector},
    query::{PointQuery, PointQueryWithLocation},
    utils::point_in_poly2d,
};
use rand::Rng;
use serde::{Deserialize, Serialize};
use terrain::{keep_path_graph_up_to_date, Convert as _, PathGraph, Terrain};
use uuid::Uuid;

pub enum Mode {
    Client,
    Server,
}

#[derive(Resource)]
pub struct GameServerConn(pub Connection);

pub struct GameEventReader(mpsc::Receiver<GameEvent>);

#[derive(Resource)]
pub struct GameClientConns(pub HashMap<PlayerId, Connection>);

pub struct GameRequestReader(mpsc::Receiver<(PlayerId, GameRequest)>);

pub struct GamePlugin<T: States + Copy> {
    pub mode: Mode,
    pub run_in_state: T,
}

#[derive(SystemSet, Debug, Clone, PartialEq, Eq, Hash)]
pub struct EventHandling;

#[derive(SystemSet, Debug, Clone, PartialEq, Eq, Hash)]
pub struct CoreGameplay;

#[derive(Resource)]
pub struct RenderDebug(pub bool);

impl<T: States + Copy> Plugin for GamePlugin<T> {
    fn build(&self, app: &mut App) {
        use bevy::ecs::schedule::ScheduleLabel;
        let active_schedule = match self.mode {
            Mode::Client => ScheduleLabel::intern(&FixedUpdate),
            Mode::Server => ScheduleLabel::intern(&Update),
        };

        app.configure_sets(
            active_schedule,
            (EventHandling, CoreGameplay.after(EventHandling)).run_if(in_state(self.run_in_state)),
        );

        match self.mode {
            Mode::Client => {
                app.add_systems(OnEnter(self.run_in_state), client_setup);
                app.add_systems(FixedUpdate, read_game_events.in_set(EventHandling));
                app.add_systems(
                    Update,
                    (
                        anim::apply_animation,
                        anim::bind_animation_controller,
                        (terrain::draw_path_graph, actor::debug_movement_path)
                            .run_if(|dbg: Res<RenderDebug>| dbg.0),
                    )
                        .run_if(in_state(self.run_in_state)),
                );
                app.insert_resource(RenderDebug(false));
            }
            Mode::Server => {
                app.add_systems(OnEnter(self.run_in_state), server_setup);
                app.add_systems(Update, read_game_requests.in_set(EventHandling));
                app.add_systems(Update, sync_position.in_set(CoreGameplay));
            }
        }

        app.insert_resource(PathGraph {
            nodes: vec![],
            terrain: vec![],
        });

        app.add_systems(active_schedule, actor::move_to_target.in_set(CoreGameplay));
        app.add_systems(
            active_schedule,
            (print_frame_time, keep_path_graph_up_to_date).in_set(CoreGameplay),
        );
        app.insert_resource(ActorKindRegistry::setup());
    }
}

#[derive(Component)]
pub struct CameraFocus;

pub const DEFAULT_CAM_POS: Vec3 = Vec3::new(0.0, -10.0, 15.0);

fn client_setup(asset_server: Res<AssetServer>, mut commands: Commands) {
    commands
        .spawn((SpatialBundle::default(), CameraFocus))
        .with_children(|builder| {
            builder.spawn(Camera3dBundle {
                projection: Projection::Perspective(PerspectiveProjection {
                    fov: std::f32::consts::FRAC_PI_4,
                    ..default()
                }),
                transform: Transform::from_translation(DEFAULT_CAM_POS * 10.0)
                    .looking_at(Vec3::ZERO, Vec3::Z),
                ..default()
            });
        });

    commands.spawn(DirectionalLightBundle {
        directional_light: DirectionalLight {
            illuminance: light_consts::lux::OVERCAST_DAY,
            ..default()
        },
        transform: Transform::default().looking_to(Vec3::new(1.0, 1.5, -2.0).normalize(), Vec3::Z),
        ..default()
    });

    let mesh = asset_server.add(Plane3d::new(Vec3::Z, Vec2::splat(50.0)).mesh().build());
    let texture = asset_server.load("mapdraft.png");
    let material = asset_server.add(StandardMaterial::from(texture));

    commands.spawn(MaterialMeshBundle {
        mesh,
        material,
        ..default()
    });

    let road_mesh = asset_server.add(Plane3d::new(Vec3::Z, Vec2::new(1.0, 1.0)).mesh().build());
    let material = asset_server.add(StandardMaterial::from_color(Color::srgb(0.5, 0.5, 0.5)));

    commands.spawn(MaterialMeshBundle {
        mesh: road_mesh.clone(),
        material: material.clone(),
        transform: Transform::from_translation(Vec3::new(-23.0, -44.0, 0.001))
            .with_scale(Vec3::new(23.0, 2.0, 1.0)),
        ..default()
    });
    commands.spawn(MaterialMeshBundle {
        mesh: road_mesh.clone(),
        material: material.clone(),
        transform: Transform::from_translation(Vec3::new(23.0, 44.0, 0.001))
            .with_scale(Vec3::new(23.0, 2.0, 1.0)),
        ..default()
    });
    commands.spawn(MaterialMeshBundle {
        mesh: road_mesh.clone(),
        material: material.clone(),
        transform: Transform::from_translation(Vec3::new(-44.0, -23.0, 0.001))
            .with_scale(Vec3::new(2.0, 23.0, 1.0)),
        ..default()
    });
    commands.spawn(MaterialMeshBundle {
        mesh: road_mesh.clone(),
        material: material.clone(),
        transform: Transform::from_translation(Vec3::new(44.0, 23.0, 0.001))
            .with_scale(Vec3::new(2.0, 23.0, 1.0)),
        ..default()
    });
    commands.spawn(MaterialMeshBundle {
        mesh: road_mesh,
        material: material.clone(),
        transform: Transform::from_translation(Vec3::new(0.0, 0.0, 0.001))
            .with_scale(Vec3::new(63.0, 2.0, 1.0))
            .with_rotation(Quat::from_rotation_z(45.0f32.to_radians())),
        ..default()
    });

    let circ_part = {
        let inner_radius = 42.0;
        let outer_radius = 46.0;
        let resolution = 32;

        let verts = |radius: f32| {
            (0..resolution).map(move |i| {
                let angle = i as f32 / (resolution - 1) as f32 * 90.0f32.to_radians();
                Vec2::from_angle(angle) * radius
            })
        };

        let inner_verts = verts(inner_radius).rev();
        let outer_verts = verts(outer_radius);

        let polyline = BoxedPolyline2d::new(outer_verts.chain(inner_verts));
        for vert in &polyline.vertices {
            println!("{vert}");
        }
        let mesh = terrain::M(polyline).mesh().build();
        asset_server.add(mesh)
    };

    commands.spawn(MaterialMeshBundle {
        mesh: circ_part.clone(),
        material: material.clone(),
        transform: Transform::from_translation(Vec3::new(0.0, 0.0, 0.001))
            .with_scale(Vec3::new(1.0, 1.0, 1.0))
            .with_rotation(Quat::from_rotation_z((-90.0f32).to_radians())),
        ..default()
    });

    commands.spawn(MaterialMeshBundle {
        mesh: circ_part.clone(),
        material: material.clone(),
        transform: Transform::from_translation(Vec3::new(0.0, 0.0, 0.01))
            .with_scale(Vec3::new(1.0, 1.0, 1.0))
            .with_rotation(Quat::from_rotation_z((90.0f32).to_radians())),
        ..default()
    });

    let mesh = asset_server.add(Cuboid::new(2.0, 2.0, 0.5).mesh().build());
    let material = asset_server.add(StandardMaterial::from_color(Color::srgb(1.0, 0.0, 0.0)));

    commands.add(|world: &mut World| {
        let conn = world.get_resource::<GameServerConn>().unwrap();
        let mut conn = Connection {
            conn: conn.0.conn.try_clone().unwrap(),
        };
        let (s, r) = mpsc::channel();
        std::thread::spawn(move || loop {
            let msg = conn.read::<GameEvent>().unwrap();
            // println!(
            //     "READ  - {}",
            //     SystemTime::now()
            //         .duration_since(SystemTime::UNIX_EPOCH)
            //         .unwrap()
            //         .as_secs_f64()
            // );
            s.send(msg).unwrap();
        });
        world.insert_non_send_resource(GameEventReader(r));
    });
}

struct TimeStats {
    frames: Vec<f32>,
    max_frames: usize,
    frames_until_update: usize,
}

impl Default for TimeStats {
    fn default() -> Self {
        Self { frames: vec![], max_frames: 60, frames_until_update: 60 }
    }
}

impl TimeStats {
    fn tick_frame(&mut self, time: f32) {
        self.frames.push(time);
        if self.frames.len() > self.max_frames {
            self.frames.remove(0);
        }
        self.frames_until_update -= 1;
        if self.frames_until_update == 0 {
            self.frames_until_update = self.max_frames;
            self.print_stats();
        }
    }

    fn print_stats(&self) {
        let avg = self.frames.iter().sum::<f32>() / self.frames.len() as f32;
        let max = self.frames.iter().fold(f32::INFINITY, |a, b| a.min(*b)); 
        let min = self.frames.iter().fold(-f32::INFINITY, |a, b| a.max(*b));

        println!("AVG: {:2.2} MIN {:2.2} MAX {:2.2}", 1.0/avg, 1.0/min, 1.0/max);
    }
}

fn print_frame_time(mut stats: Local<TimeStats>, time: Res<Time<Real>>) {
    stats.tick_frame(time.delta_seconds());
}

fn read_game_events(
    reader: NonSend<GameEventReader>,
    actor_registry: Res<ActorKindRegistry>,
    asset_server: Res<AssetServer>,
    time: Res<Time<Real>>,
    mut commands: Commands,
) {
    while let Ok(event) = reader.0.try_recv() {
        match event {
            GameEvent::ActorSpawned(spawn) => {
                if let Some(builder) = actor_registry.map.get(&spawn.kind_id) {
                    builder.build(spawn, &asset_server, &mut commands)
                }
            }
            GameEvent::MovementTargetSet(actor, mut pos) => {
                // println!(
                //     "EVENT - {}",
                //     SystemTime::now()
                //         .duration_since(SystemTime::UNIX_EPOCH)
                //         .unwrap()
                //         .as_secs_f64()
                // );

                commands.add(move |world: &mut World| {
                    for (mut target, id) in world
                        .query::<(&mut MovementTarget, &ActorId)>()
                        .iter_mut(world)
                    {
                        if *id != actor {
                            continue;
                        }

                        target.0 = Some(pos);
                        break;
                    }
                    // println!(
                    //     "REQUE - {}",
                    //     SystemTime::now()
                    //         .duration_since(SystemTime::UNIX_EPOCH)
                    //         .unwrap()
                    //         .as_secs_f64()
                    // );
                });
            }
            GameEvent::PositionSync(actor, pos) => {
                commands.add(move |world: &mut World| {
                    for (mut client_pos, target, id) in
                        world.query::<(&mut Transform, Option<&mut MovementTarget>, &ActorId)>().iter_mut(world)
                    {
                        if *id != actor {
                            continue;
                        }

                        let diff = (client_pos.translation.xy() - pos).length();
                        println!("Sync diff: {diff}");
                        if diff < 0.1 {
                            continue;
                        }

                        client_pos.translation = pos.extend(client_pos.translation.z);
                        if let Some(mut target) = target {
                            println!("UPDATE TARGET");
                            target.set_changed();
                        }
                        break;
                    }
                });
            }
        }
    }
}

fn server_setup(mut connections: ResMut<GameClientConns>, mut commands: Commands) {
    commands.add(|world: &mut World| {
        let conns = world.get_resource::<GameClientConns>().unwrap();
        let (s, r) = mpsc::channel();
        for (player_id, conn) in &conns.0 {
            let player_id = *player_id;
            let mut conn = Connection {
                conn: conn.conn.try_clone().unwrap(),
            };
            let s = s.clone();
            std::thread::spawn(move || loop {
                match conn.read::<GameRequest>() {
                    Ok(msg) => {
                        s.send((player_id, msg)).unwrap();
                    }
                    Err(_) => {
                        s.send((player_id, GameRequest::Disconnect)).unwrap();
                        break;
                    }
                }
            });
        }
        world.insert_non_send_resource(GameRequestReader(r));
    });

    let mut spawns = vec![];

    spawns.push(NexusSpawner.spawn(Vec2::splat(-42.0), NexusData, &mut commands));
    spawns.push(TowerSpawner.spawn(Vec2::new(-20.0, -40.0), TowerData, &mut commands));
    spawns.push(TowerSpawner.spawn(Vec2::new(-25.0, -25.0), TowerData, &mut commands));
    spawns.push(TowerSpawner.spawn(Vec2::new(-40.0, -20.0), TowerData, &mut commands));

    for (player, _) in &connections.0 {
        let data = Champ1Data {
            controller: UnitControllerInfo::Player(*player),
        };
        let spawn = Champ1Spawner.spawn(Vec2::splat(-50.0), data, &mut commands);

        spawns.push(spawn);
    }

    for mut spawn in spawns {
        for (_, conn) in &mut connections.0 {
            let msg = GameEvent::ActorSpawned(spawn);
            conn.write(&msg).unwrap();
            let GameEvent::ActorSpawned(x) = msg else {
                unreachable!()
            };
            spawn = x;
        }
    }
}

fn read_game_requests(
    reader: NonSend<GameRequestReader>,
    mut q: Query<(&ActorId, &UnitController, &mut MovementTarget)>,
    graph: Res<PathGraph>,
    mut connections: ResMut<GameClientConns>,
    mut exit: EventWriter<AppExit>,
) {
    while let Ok((id, request)) = reader.0.try_recv() {
        match request {
            GameRequest::SetMovementTarget(mut pos) => {
                // println!("TARGET {pos}");
                for (terrain, isometry) in &graph.terrain {
                    // println!("Checking terrain");
                    if terrain.aabb(isometry).contains_local_point(&pos.conv()) {
                        // println!("AABB contains point");
                        let points = terrain.segments().map(|x| x.a).collect::<Vec<_>>();
                        if point_in_poly2d(
                            &isometry.inverse_transform_point(&pos.conv()),
                            points.as_slice(),
                        ) {
                            // println!("Terrain contains point");

                            let (proj, (segment_id, loc)) = terrain.project_point_and_get_location(
                                &isometry,
                                &pos.conv(),
                                true,
                            );
                            let segment = terrain.segment(segment_id);
                            pos = (proj.point + *segment.normal().unwrap() * terrain::EPS).conv();
                        }
                    }
                }
                // println!("TARGET {pos}");

                let (&actor, _, mut target) = q
                    .iter_mut()
                    .find(|x| matches!(x.1, UnitController::Player(p) if *p == id))
                    .unwrap();

                target.0 = Some(pos);

                for conn in connections.0.values_mut() {
                    conn.write(&GameEvent::MovementTargetSet(actor, pos))
                        .unwrap();
                }
            }
            GameRequest::Disconnect => {
                connections.0.remove(&id);

                if connections.0.is_empty() {
                    exit.send(AppExit::Success);
                }
            }
        }
    }
}

fn sync_position(
    mut timer: Local<f32>,
    q: Query<(&ActorId, &Transform)>,
    mut conn: ResMut<GameClientConns>,
    time: Res<Time>,
) {
    *timer += time.delta_seconds();

    if *timer > 1.0 {
        *timer -= 1.0;

        for (actor, trans) in &q {
            for conn in conn.0.values_mut() {
                conn.write(&GameEvent::PositionSync(*actor, trans.translation.xy()))
                    .unwrap();
            }
        }
    }
}

#[derive(Component)]
pub struct Actor;

#[derive(Debug, Serialize, Deserialize)]
pub enum GameEvent {
    ActorSpawned(ActorSpawned),
    MovementTargetSet(ActorId, Vec2),
    PositionSync(ActorId, Vec2),
}

#[derive(Debug, Serialize, Deserialize)]
pub enum GameRequest {
    SetMovementTarget(Vec2),
    Disconnect,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ActorSpawned {
    actor_id: ActorId,
    pos: Vec2,
    actor_type: ActorType,
    kind_id: ActorKindId,
    data: Vec<u8>,
}

#[derive(Debug, Serialize, Deserialize)]
pub enum OtherData {
    Bytes(Vec<u8>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ActorKindId(pub Uuid);

#[derive(Debug, Serialize, Deserialize)]
pub enum ActorType {
    Unit(UnitData),
    Projectile(ProjectileData),
    Other(OtherData),
}

#[derive(Debug, Serialize, Deserialize)]
pub struct UnitData {
    team: Team,
    stats: Stats,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ProjectileData {}

#[derive(Resource)]
pub struct ActorKindRegistry {
    map: HashMap<ActorKindId, Box<dyn ActorBuilder>>,
}

impl ActorKindRegistry {
    fn setup() -> Self {
        let mut registry = Self {
            map: HashMap::new(),
        };

        registry.add(Champ1Builder);
        registry.add(TowerBuilder);
        registry.add(NexusBuilder);

        registry
    }

    fn add<A: ActorBuilder + 'static>(&mut self, builder: A) {
        println!(
            "{} -- {:?}",
            std::any::type_name::<A>(),
            builder.actor_kind_id()
        );
        if self
            .map
            .insert(builder.actor_kind_id(), Box::new(builder))
            .is_some()
        {
            panic!("duplicate builder inserted");
        }
    }
}

pub trait ActorBuilder: Send + Sync {
    fn actor_kind_id(&self) -> ActorKindId;
    fn build(&self, data: ActorSpawned, asset_server: &AssetServer, commands: &mut Commands);
}

pub trait ActorSpawner {
    type Data;
    fn spawn(&self, pos: Vec2, data: Self::Data, commands: &mut Commands) -> ActorSpawned;
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Stats {
    max_health: f32,
    max_mana: f32,
    movement_speed: f32,
    attack_speed: f32,
    attr_a: f32,
    attr_b: f32,
    attr_c: f32,
    resist_a: f32,
    resist_b: f32,
    resist_c: f32,
}

pub macro actor_id($id:ident = $uuid:literal) {
    pub const $id: ActorKindId = ActorKindId(match Uuid::try_parse($uuid) {
        Ok(v) => v,
        Err(_) => panic!("Invalid UUID"),
    });
}
