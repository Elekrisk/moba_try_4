#![feature(never_type)]
#![feature(anonymous_lifetime_in_impl_trait)]
#![feature(decl_macro)]
#![feature(get_many_mut)]
#![feature(try_blocks)]

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
        attack::AttackTarget,
        champion::champ_1::{AutoBuilder, Champ1Builder, Champ1Data, Champ1Spawner},
        UnitController, UnitControllerInfo,
    },
    ActorId, ActorMap, ActorPlugin, MovementTarget,
};
use bevy::{
    asset::AssetEvents,
    ecs::{intern::Interned, schedule::ScheduleLabel, system::RunSystemOnce},
    gizmos::gizmos::GizmoStorage,
    prelude::*,
    utils::HashMap,
};
use lobby_server::{Connection, PlayerId, Team};
use parry2d::{
    math::{Isometry, Point, Vector},
    query::{PointProjection, PointQuery, PointQueryWithLocation},
    utils::point_in_poly2d,
};
use rand::Rng;
use serde::{Deserialize, Serialize};
use terrain::{keep_path_graph_up_to_date, Convert as _, CustomTerrain, PathGraph, Terrain};
use uuid::Uuid;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Mode {
    Client,
    Server,
}

impl Mode {
    fn schedule(&self) -> Interned<dyn ScheduleLabel> {
        match self {
            Mode::Client => ScheduleLabel::intern(&FixedUpdate),
            Mode::Server => ScheduleLabel::intern(&Update),
        }
    }
}

#[derive(Resource)]
pub struct GameServerConn(pub Connection);

pub struct GameEventReader(mpsc::Receiver<GameEvent>);

#[derive(Resource)]
pub struct GameClientConns(pub HashMap<PlayerId, Connection>);

impl GameClientConns {
    pub fn send(&mut self, id: PlayerId, event: &GameEvent) {
        self.0.get_mut(&id).unwrap().write(event).unwrap();
    }

    pub fn broadcast(&mut self, event: &GameEvent) {
        for conn in self.0.values_mut() {
            conn.write(event).unwrap();
        }
    }

    pub fn broadcast_ignore(&mut self, event: &GameEvent, ignore: PlayerId) {
        for (id, conn) in &mut self.0 {
            if *id == ignore {
                continue;
            }

            conn.write(event).unwrap();
        }
    }
}

#[derive(Resource)]
pub struct PlayerTeams(pub HashMap<PlayerId, Team>);

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
        app.add_plugins(ActorPlugin(self.mode));

        let active_schedule = self.mode.schedule();

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
            (keep_path_graph_up_to_date).in_set(CoreGameplay),
        );
        app.insert_resource(ActorKindRegistry::setup());

        app.add_systems(Startup, setup_observer);
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
        Self {
            frames: vec![],
            max_frames: 60,
            frames_until_update: 60,
        }
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

        println!(
            "AVG: {:2.2} MIN {:2.2} MAX {:2.2}",
            1.0 / avg,
            1.0 / min,
            1.0 / max
        );
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
                commands.add(move |world: &mut World| {
                    for (mut target, attack, id) in world
                        .query::<(&mut MovementTarget, Option<&mut AttackTarget>, &ActorId)>()
                        .iter_mut(world)
                    {
                        if *id != actor {
                            continue;
                        }

                        target.0 = Some(pos);
                        if let Some(mut target) = attack {
                            target.0 = None;
                        }
                        break;
                    }
                });
            }
            GameEvent::PositionSync(actor, pos) => {
                commands.add(move |world: &mut World| {
                    for (mut client_pos, target, id) in world
                        .query::<(&mut Transform, Option<&mut MovementTarget>, &ActorId)>()
                        .iter_mut(world)
                    {
                        if *id != actor {
                            continue;
                        }

                        let diff = (client_pos.translation.xy() - pos).length();
                        // println!("Sync diff: {diff}");
                        if diff < 0.1 {
                            continue;
                        }

                        client_pos.translation = pos.extend(client_pos.translation.z);
                        if let Some(mut target) = target {
                            // println!("UPDATE TARGET");
                            target.set_changed();
                        }
                        break;
                    }
                });
            }
            GameEvent::CreateTerrain(actor, terrain, pos) => {
                commands.spawn((
                    actor,
                    Terrain::from_vertices(terrain),
                    SpatialBundle::from_transform(Transform::from_translation(pos.extend(0.0))),
                ));
            }
            GameEvent::EditTerrain(actor, new_terrain) => {
                commands.add(move |world: &mut World| {
                    let map = world.resource::<ActorMap>();
                    let e = *map.0.get(&actor).unwrap();
                    *world.entity_mut(e).get_mut::<Terrain>().unwrap() =
                        Terrain::from_vertices(new_terrain);
                });
            }
            GameEvent::DeleteActor(actor) => {
                commands.add(move |world: &mut World| {
                    let map = world.resource::<ActorMap>();
                    let e = *map.0.get(&actor).unwrap();
                    world.entity_mut(e).despawn_recursive();
                });
            }
            GameEvent::AttackTargetSet(a, b) => {
                commands.add(move |world: &mut World| {
                    let map = world.resource::<ActorMap>();
                    let e_a = *map.0.get(&a).unwrap();
                    let e_b = *map.0.get(&b).unwrap();
                    
                    world.entity_mut(e_a).get_mut::<AttackTarget>().unwrap().0 = Some(e_b);
                });
            }
        }
    }
}

fn server_setup(
    mut connections: ResMut<GameClientConns>,
    player_teams: Res<PlayerTeams>,
    mut commands: Commands,
) {
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
                        println!("{msg:#?}");
                        s.send((player_id, msg)).unwrap();
                    }
                    Err(_) => {
                        s.send((player_id, GameRequest::Disconnect(Disconnect)))
                            .unwrap();
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
        let team = *player_teams.0.get(player).unwrap();
        let data = Champ1Data {
            controller: UnitControllerInfo::Player(*player),
            team,
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

    commands.add(terrain::LoadTerrain);
}

fn read_game_requests(
    reader: NonSend<GameRequestReader>,
    q: Query<(Entity, &UnitController)>,
    mut commands: Commands,
) {
    while let Ok((id, req)) = reader.0.try_recv() {
        let e = q
            .iter()
            .find(|x| matches!(x.1, UnitController::Player(p) if *p == id))
            .map(|x| x.0)
            .unwrap_or(Entity::PLACEHOLDER);

        macro trigger($req:ident) {
            commands.trigger_targets(
                WithPlayer {
                    player: id,
                    event: $req,
                },
                e,
            )
        }

        match req {
            GameRequest::SetMovementTarget(req) => trigger!(req),
            GameRequest::SetAttackTarget(req) => trigger!(req),
            GameRequest::Disconnect(req) => trigger!(req),
            GameRequest::Editor(req) => match req {
                EditorRequest::CreateTerrain(req) => trigger!(req),
                EditorRequest::MoveActor(req) => trigger!(req),
                EditorRequest::DeleteActor(req) => trigger!(req),
                EditorRequest::EditTerrain(req) => trigger!(req),
                EditorRequest::SaveTerrain(req) => trigger!(req),
                EditorRequest::ReloadTerrain(req) => trigger!(req),
            },
        }
    }
}
fn setup_observer(mut commands: Commands) {
    commands.observe(server_request_set_movement_target);
    commands.observe(server_request_set_attack_target);
    commands.observe(server_request_disconnect);
    commands.observe(server_request_create_terrain);
    commands.observe(server_request_edit_terrain);
    commands.observe(server_request_move_actor);
    commands.observe(server_request_delete_actor);
    commands.observe(server_request_save_terrain);
    commands.observe(server_request_reload_terrain);
}

fn server_request_set_movement_target(
    trigger: Trigger<WithPlayer<SetMovementTarget>>,
    graph: Res<PathGraph>,
    mut q: Query<(&ActorId, &mut MovementTarget, Option<&mut AttackTarget>)>,
    mut connections: ResMut<GameClientConns>,
) {
    let mut pos = trigger.event().event.0;
    if let Ok((actor, mut target, attack)) = q.get_mut(trigger.entity()) {
        for (terrain, isometry) in &graph.terrain {
            if terrain.aabb(isometry).contains_local_point(&pos.conv()) {
                let points = terrain.segments().map(|x| x.a).collect::<Vec<_>>();
                if point_in_poly2d(
                    &isometry.inverse_transform_point(&pos.conv()),
                    points.as_slice(),
                ) {
                    let (proj, (segment_id, loc)) =
                        terrain.project_point_and_get_location(&isometry, &pos.conv(), true);
                    let segment = terrain.segment(segment_id);

                    let normal = match loc {
                        parry2d::shape::SegmentPointLocation::OnVertex(i) => {
                            let other_id = match i {
                                0 => {
                                    if segment_id == 0 {
                                        terrain.num_segments() as u32 - 1
                                    } else {
                                        segment_id - 1
                                    }
                                }
                                1 => (segment_id + 1) % terrain.num_segments() as u32,
                                _ => unreachable!(),
                            };
                            let other = terrain.segment(other_id);
                            (*other.normal().unwrap() + *segment.normal().unwrap()).normalize()
                        }
                        parry2d::shape::SegmentPointLocation::OnEdge(_) => {
                            *segment.normal().unwrap()
                        }
                    };
                    println!("{loc:#?}");
                    pos = (proj.point + normal * terrain::EPS).conv();
                }
            }
        }

        target.0 = Some(pos);
        if let Some(mut attack) = attack {
            attack.0 = None;
        }

        connections.broadcast(&GameEvent::MovementTargetSet(*actor, pos));
    }
}

fn server_request_set_attack_target(
    trigger: Trigger<WithPlayer<SetAttackTarget>>,
    actor_map: Res<ActorMap>,
    mut q: Query<(&ActorId, &mut AttackTarget)>,
    mut connections: ResMut<GameClientConns>
) {
    let ev = &trigger.event().event;
    let entity = actor_map.0.get(&ev.0).unwrap();
    let (actor, mut target) = q.get_mut(trigger.entity()).unwrap();
    target.0 = Some(*entity);
    connections.broadcast(&GameEvent::AttackTargetSet(*actor, ev.0));
}

fn server_request_disconnect(
    trigger: Trigger<WithPlayer<Disconnect>>,
    mut connections: ResMut<GameClientConns>,
    mut exit: EventWriter<AppExit>,
) {
    let ev = trigger.event();
    connections.0.remove(&ev.player);

    if connections.0.len() == 0 {
        exit.send(AppExit::Success);
    }
}

fn server_request_create_terrain(
    trigger: Trigger<WithPlayer<CreateTerrain>>,
    mut connections: ResMut<GameClientConns>,
    mut commands: Commands,
) {
    let ev = trigger.event();

    let actor_id = ActorId(Uuid::new_v4());

    commands.spawn((
        CustomTerrain,
        actor_id,
        Terrain::from_vertices(ev.event.terrain.iter().copied()),
        TransformBundle::from_transform(Transform::from_translation(ev.event.pos.extend(0.0))),
    ));

    connections.broadcast(&GameEvent::CreateTerrain(
        actor_id,
        ev.event.terrain.clone(),
        ev.event.pos,
    ));
}

fn server_request_edit_terrain(
    trigger: Trigger<WithPlayer<EditTerrain>>,
    actor_map: Res<ActorMap>,
    mut q: Query<&mut Terrain>,
    mut connections: ResMut<GameClientConns>,
) {
    let ev = trigger.event();
    let e = actor_map.0.get(&ev.event.actor).unwrap();
    *q.get_mut(*e).unwrap() = Terrain::from_vertices(ev.event.new_terrain.iter().copied());
    connections.broadcast_ignore(
        &GameEvent::EditTerrain(ev.event.actor, ev.event.new_terrain.clone()),
        ev.player,
    );
}

fn server_request_move_actor(
    trigger: Trigger<WithPlayer<MoveActor>>,
    mut q: Query<&mut Transform>,
    actor_map: Res<ActorMap>,
    mut connections: ResMut<GameClientConns>,
) {
    let ev = trigger.event();
    let e = actor_map.0.get(&ev.event.actor).unwrap();
    q.get_mut(*e).unwrap().translation = ev.event.new_pos.extend(0.0);
    for (player_id, conn) in connections.0.iter_mut() {
        if *player_id == ev.player {
            continue;
        }
        conn.write(&GameEvent::PositionSync(ev.event.actor, ev.event.new_pos))
            .unwrap();
    }
}

fn server_request_delete_actor(
    trigger: Trigger<WithPlayer<DeleteActor>>,
    actor_map: Res<ActorMap>,
    mut connections: ResMut<GameClientConns>,
    mut commands: Commands,
) {
    let ev = trigger.event();
    let e = actor_map.0.get(&ev.event.actor).unwrap();
    commands.entity(*e).despawn_recursive();
    connections.broadcast(&GameEvent::DeleteActor(ev.event.actor));
}

fn server_request_save_terrain(_: Trigger<WithPlayer<SaveTerrain>>, mut commands: Commands) {
    commands.add(terrain::SaveTerrain);
}
fn server_request_reload_terrain(_: Trigger<WithPlayer<ReloadTerrain>>, mut commands: Commands) {
    commands.add(terrain::LoadTerrain);
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
    AttackTargetSet(ActorId, ActorId),
    PositionSync(ActorId, Vec2),
    CreateTerrain(ActorId, Vec<Vec2>, Vec2),
    EditTerrain(ActorId, Vec<Vec2>),
    DeleteActor(ActorId),
}

#[derive(Debug, Serialize, Deserialize)]
pub enum GameRequest {
    SetMovementTarget(SetMovementTarget),
    SetAttackTarget(SetAttackTarget),
    Disconnect(Disconnect),
    Editor(EditorRequest),
}

#[derive(Debug, Serialize, Deserialize, Event)]
pub struct SetMovementTarget(pub Vec2);

#[derive(Debug, Serialize, Deserialize, Event)]
pub struct SetAttackTarget(pub ActorId);

#[derive(Debug, Serialize, Deserialize, Event)]
pub struct Disconnect;

#[derive(Debug, Serialize, Deserialize)]
pub enum EditorRequest {
    CreateTerrain(CreateTerrain),
    MoveActor(MoveActor),
    DeleteActor(DeleteActor),
    EditTerrain(EditTerrain),
    SaveTerrain(SaveTerrain),
    ReloadTerrain(ReloadTerrain),
}

#[derive(Debug, Serialize, Deserialize, Event)]
pub struct WithPlayer<E> {
    pub player: PlayerId,
    pub event: E,
}

#[derive(Debug, Serialize, Deserialize, Event)]
pub struct CreateTerrain {
    pub terrain: Vec<Vec2>,
    pub pos: Vec2,
}
#[derive(Debug, Serialize, Deserialize, Event)]
pub struct MoveActor {
    pub actor: ActorId,
    pub new_pos: Vec2,
}
#[derive(Debug, Serialize, Deserialize, Event)]
pub struct DeleteActor {
    pub actor: ActorId,
}
#[derive(Debug, Serialize, Deserialize, Event)]
pub struct EditTerrain {
    pub actor: ActorId,
    pub new_terrain: Vec<Vec2>,
}
#[derive(Debug, Serialize, Deserialize, Event)]
pub struct SaveTerrain;
#[derive(Debug, Serialize, Deserialize, Event)]
pub struct ReloadTerrain;

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
        registry.add(AutoBuilder);
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
    type SendData: Serialize + for<'de> Deserialize<'de>;
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
