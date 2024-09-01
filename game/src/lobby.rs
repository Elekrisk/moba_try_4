use std::{net::{TcpStream, ToSocketAddrs}, sync::mpsc};

use crate::{
    button::{ButtonAction, ButtonPlugin, UiMyButtonExt as _},
    Options, SkipLobby, TopLevelState,
};

use bevy::{prelude::*, utils::HashMap};
use engine::GameServerConn;
use game_server::GameToGameServer;
use lobby_server::{
    ClientMessage, Connection, Lobby, LobbyId, Player, PlayerId, ServerMessage, Team,
};
use sickle_ui::{
    prelude::*, ui_commands::UpdateStatesExt, widgets::layout::label::SetLabelTextExt as _,
};

pub struct LobbyMenuPlugin(pub Options);

impl Plugin for LobbyMenuPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(ButtonPlugin);
        app.add_event::<LobbyStateChange>()
            .add_event::<JoinedLobby>();
        app.insert_resource(CachedPlayerInfo(HashMap::new()))
            .insert_resource(self.0.clone());
        app.add_systems(OnEnter(TopLevelState::LobbyMenu), setup);
        app.add_systems(
            Update,
            listen_for_events.run_if(in_state(TopLevelState::LobbyMenu)),
        );
        app.add_systems(
            Update,
            update_lobby_list.run_if(
                in_state(TopLevelState::LobbyMenu)
                    .and_then(resource_exists_and_changed::<LobbyList>),
            ),
        );
        app.add_systems(
            Update,
            (create_lobby_screen, apply_deferred, on_lobby_state_changed)
                .chain()
                .run_if(in_state(TopLevelState::LobbyMenu)),
        );

        app.add_systems(OnExit(TopLevelState::LobbyMenu), cleanup);
    }
}

fn cleanup(
    q: Query<Entity, Or<((With<Node>, Without<Parent>), With<Camera2d>)>>,
    mut commands: Commands,
) {
    for e in &q {
        commands.entity(e).despawn_recursive();
    }
}

fn setup(options: Res<Options>, skip_lobby: Option<Res<SkipLobby>>, mut commands: Commands) {
    let mut addr_string = options.lobby_ip.clone();
    if !addr_string.contains(':') {
        addr_string.push_str(":27300");
    }
    let addr = match addr_string.to_socket_addrs() {
        Ok(mut addrs) => addrs.find(|addr| addr.is_ipv4()).unwrap(),
        Err(e) => {
            eprintln!("Invalid address {} ; {}", addr_string, e);
            commands.add(|world: &mut World| {
                world.send_event(AppExit::Error(1.try_into().unwrap()));
            });
            return;
        }
    };

    println!("Connecting to {addr_string} @ {addr}...");

    let stream = match TcpStream::connect(addr) {
        Ok(stream) => stream,
        Err(e) => {
            eprintln!("Cannot connect to server at {} ; {}", addr, e);
            commands.add(|world: &mut World| {
                world.send_event(AppExit::Error(1.try_into().unwrap()));
            });
            return;
        }
    };

    let (s, recv) = mpsc::channel();

    let mut conn = Connection {
        conn: stream.try_clone().unwrap(),
    };

    conn.write(&ClientMessage::SetName(options.username.clone()))
        .unwrap();
    conn.write(&ClientMessage::GetMyId).unwrap();
    conn.write(&ClientMessage::GetLobbyList).unwrap();

    if skip_lobby.is_some() {
        conn.write(&ClientMessage::CreateNewLobby).unwrap();
        conn.write(&ClientMessage::StartGame).unwrap();
    }

    std::thread::spawn(move || loop {
        let msg = conn.read().unwrap();
        s.send(msg).unwrap();
    });

    commands.add(|world: &mut World| {
        world.insert_non_send_resource(LobbyServerConn {
            conn: Connection { conn: stream },
            recv,
        })
    });

    commands.spawn(Camera2dBundle { ..default() });

    commands
        .ui_builder(UiRoot)
        .column(|builder| {
            builder.tab_container(|builder| {
                builder
                    .add_tab("Playing".into(), playing_menu)
                    .add_tab("Settings".into(), settings_menu);
            });
        })
        .style()
        .width(Val::Percent(100.0));
}

fn playing_menu(builder: &mut UiBuilder<Entity>) {
    builder.container(
        (NodeBundle::default(), UiContextRoot, PlayingRoot),
        build_lobby_list,
    );
}

fn build_lobby_list(builder: &mut UiBuilder<Entity>) {
    builder.column(|builder| {
        builder.row(|builder| {
            builder.my_button(
                ButtonAction::world(|world| {
                    world
                        .non_send_resource_mut::<LobbyServerConn>()
                        .conn
                        .write(&ClientMessage::CreateNewLobby)
                        .unwrap()
                }),
                |builder| {
                    builder.label(LabelConfig {
                        label: "New Lobby".into(),
                        color: Color::WHITE,
                        ..default()
                    });
                },
            );
            builder.my_button(
                ButtonAction::world(|world| {
                    world
                        .non_send_resource_mut::<LobbyServerConn>()
                        .conn
                        .write(&ClientMessage::GetLobbyList)
                        .unwrap()
                }),
                |builder| {
                    builder.label(LabelConfig {
                        label: "Refresh List".into(),
                        color: Color::WHITE,
                        ..default()
                    });
                },
            );
        });

        builder.container(
            (NodeBundle::default(), UiContextRoot, LobbyListRoot),
            |_| {},
        );
    });
}

#[derive(Component)]
struct LobbyListRoot;

#[derive(Component)]
struct PlayingRoot;

fn settings_menu(builder: &mut UiBuilder<Entity>) {
    builder.column(|builder| {
        builder.my_button(
            ButtonAction::world(|w| {
                w.send_event(AppExit::Success);
            }),
            |builder| {
                builder.label(LabelConfig {
                    label: "Exit".into(),
                    color: Color::WHITE,
                    ..default()
                });
            },
        );
    });
}

fn listen_for_events(
    options: Res<Options>,
    mut transaction_info: Local<HashMap<PlayerId, String>>,
    mut conn: Option<NonSendMut<LobbyServerConn>>,
    mut cache: ResMut<CachedPlayerInfo>,
    mut lobby_joined: EventWriter<JoinedLobby>,
    mut events: EventWriter<LobbyStateChange>,
    my_id: Option<Res<MyPlayerId>>,
    mut commands: Commands,
) {
    let Some(mut conn) = conn else {
        return;
    };
    while let Ok(msg) = conn.recv.try_recv() {
        match msg {
            ServerMessage::LobbyJoined(lobby) => conn
                .conn
                .write(&ClientMessage::GetLobbyInfo(lobby))
                .unwrap(),
            ServerMessage::LobbyLeft => {
                commands.remove_resource::<CurrentLobby>();
                commands.add(|world: &mut World| {
                    let root = world
                        .query_filtered::<Entity, With<PlayingRoot>>()
                        .single(world);
                    world.entity_mut(root).despawn_descendants();
                    let mut binding = world.commands();
                    let mut builder = binding.ui_builder(root);
                    build_lobby_list(&mut builder);
                    world.flush();
                    world
                        .non_send_resource_mut::<LobbyServerConn>()
                        .conn
                        .write(&ClientMessage::GetLobbyList)
                        .unwrap();
                });
            }
            ServerMessage::LobbyInfo(lobby) => {
                commands.insert_resource(CurrentLobby {
                    lobby: lobby.clone(),
                });
                for player in lobby.players.values().flatten() {
                    conn.conn
                        .write(&ClientMessage::GetPlayerInfo(*player))
                        .unwrap();
                }
                lobby_joined.send(JoinedLobby(lobby));
            }
            ServerMessage::LobbyList(lobbies) => commands.insert_resource(LobbyList(lobbies)),
            ServerMessage::PlayerNameUpdate(id, new_name) => {
                cache.0.get_mut(&id).unwrap().name = new_name.clone();
                commands.add(move |world: &mut World| {
                    if let Some((e, _)) = world
                        .query::<(Entity, &PlayerEntry)>()
                        .iter(world)
                        .find(|(_, p)| p.0 == id)
                    {
                        world.commands().entity(e).set_label_text(new_name);
                    }
                });
            }
            ServerMessage::PlayerJoinedLobby(player) => {
                transaction_info.insert(player, "JoinedLobby".into());
                conn.conn
                    .write(&ClientMessage::GetPlayerInfo(player))
                    .unwrap();
            }
            ServerMessage::PlayerLeftLobby(player) => {
                events.send(LobbyStateChange::PlayerLeft(player));
                cache.0.remove(&player);
            }
            ServerMessage::PlayerSwitchedTeam(player) => {}
            ServerMessage::PlayerInfo { player, team } => {
                if let Some(x) = transaction_info.remove(&player.id) {
                    if x == "JoinedLobby" {
                        events.send(LobbyStateChange::PlayerJoined(player.id, team.unwrap()));
                    }
                }

                let name = player.name.clone();
                let player_id = player.id;
                cache.0.insert(player.id, player);
                commands.add(move |world: &mut World| {
                    if let Some((e, _)) = world
                        .query::<(Entity, &PlayerEntry)>()
                        .iter(world)
                        .find(|(_, p)| p.0 == player_id)
                    {
                        world.commands().entity(e).set_label_text(name);
                    }
                });
            }
            ServerMessage::YourPlayerId(id) => {
                commands.insert_resource(MyPlayerId(id));
                conn.conn.write(&ClientMessage::GetPlayerInfo(id)).unwrap();
            }
            ServerMessage::JoinGame(addr, token) => {
                let stream = TcpStream::connect(addr).unwrap();
                let mut conn = Connection { conn: stream };
                conn.write(&GameToGameServer::Identify {
                    id: my_id.as_ref().unwrap().0,
                    token,
                })
                .unwrap();
                commands.insert_resource(GameServerConn(conn));
                commands.next_state(TopLevelState::InGame);
            }
        }
    }
}

#[derive(Resource, Clone, Copy)]
pub struct MyPlayerId(pub PlayerId);

#[derive(Resource)]
struct LobbyList(Vec<(LobbyId, usize)>);

#[derive(Resource)]
struct CurrentLobby {
    lobby: Lobby,
}

#[derive(Resource)]
struct CachedPlayerInfo(HashMap<PlayerId, Player>);

#[derive(Component)]
struct PlayerEntry(PlayerId);
#[derive(Component)]
struct PlayerList(Team);

#[derive(Event)]
struct JoinedLobby(Lobby);

fn create_lobby_screen(
    mut lobby_joined: EventReader<JoinedLobby>,
    cache: Res<CachedPlayerInfo>,
    playing_root: Query<Entity, With<PlayingRoot>>,
    mut commands: Commands,
) {
    for JoinedLobby(lobby) in lobby_joined.read() {
        let root = playing_root.single();
        commands.entity(root).despawn_descendants();
        commands.ui_builder(root).column(|builder| {
            builder.row(|builder| {
                builder.my_button(
                    ButtonAction::world(|world| {
                        world
                            .non_send_resource_mut::<LobbyServerConn>()
                            .conn
                            .write(&ClientMessage::LeaveLobby)
                            .unwrap();
                    }),
                    |builder| {
                        builder.label(LabelConfig {
                            label: "Leave Lobby".into(),
                            ..default()
                        });
                    },
                );
                builder.my_button(
                    ButtonAction::world(|world| {
                        world
                            .non_send_resource_mut::<LobbyServerConn>()
                            .conn
                            .write(&ClientMessage::StartGame)
                            .unwrap();
                    }),
                    |builder| {
                        builder.label(LabelConfig {
                            label: "Play".into(),
                            ..default()
                        });
                    },
                );
            });
            builder.row(|builder| {
                for team in (0..lobby.settings.team_count).map(Team) {
                    builder
                        .column(|builder| {
                            builder.label(LabelConfig {
                                label: format!("Team {team}"),
                                ..default()
                            });

                            for player in lobby.players.get(&team).unwrap_or(&vec![]) {
                                build_player_entry(&cache, *player, builder);
                            }
                        })
                        .insert(PlayerList(team));
                }
            });
        });
    }
}

fn build_player_entry(cache: &CachedPlayerInfo, player: PlayerId, builder: &mut UiBuilder<Entity>) {
    let name = cache
        .0
        .get(&player)
        .map(|p| p.name.clone())
        .unwrap_or_else(|| player.0.to_string());
    builder
        .label(LabelConfig {
            label: name,
            ..default()
        })
        .insert(PlayerEntry(player));
}

#[derive(Event)]
enum LobbyStateChange {
    PlayerLeft(PlayerId),
    PlayerJoined(PlayerId, Team),
    PlayerMoved(PlayerId),
}

fn on_lobby_state_changed(
    mut events: EventReader<LobbyStateChange>,
    cache: Res<CachedPlayerInfo>,
    player_lists: Query<(Entity, &PlayerList)>,
    player_entries: Query<(Entity, &PlayerEntry)>,
    mut commands: Commands,
) {
    if events.is_empty() {
        return;
    }

    for event in events.read() {
        match event {
            LobbyStateChange::PlayerLeft(player) => {
                let (e, _) = player_entries.iter().find(|(_, p)| p.0 == *player).unwrap();
                commands.entity(e).despawn_recursive();
            }
            LobbyStateChange::PlayerJoined(player, team) => {
                let (e, _) = player_lists.iter().find(|(_, p)| p.0 == *team).unwrap();
                let mut builder = commands.ui_builder(e);
                build_player_entry(&cache, *player, &mut builder);
            }
            LobbyStateChange::PlayerMoved(player) => {}
        }
    }
}

fn update_lobby_list(
    lobby_list: Res<LobbyList>,
    q: Query<Entity, With<LobbyListRoot>>,
    mut commands: Commands,
) {
    let root = q.single();
    let mut ec = commands.entity(root);
    ec.despawn_descendants();
    commands.ui_builder(root).column(|builder| {
        for (id, count) in &lobby_list.0 {
            create_lobby_list_entry(*id, *count, builder);
        }
    });
}

fn create_lobby_list_entry(lobby: LobbyId, count: usize, builder: &mut UiBuilder<Entity>) {
    builder.row(|builder| {
        builder.label(LabelConfig {
            label: format!("{}", lobby.0),
            ..default()
        });
        builder.label(LabelConfig {
            label: count.to_string(),
            ..default()
        });
        builder.my_button(
            ButtonAction::world(move |world| {
                world
                    .non_send_resource_mut::<LobbyServerConn>()
                    .conn
                    .write(&ClientMessage::JoinLobby(lobby))
                    .unwrap();
            }),
            |builder| {
                builder.label(LabelConfig {
                    label: "Join".into(),
                    ..default()
                });
            },
        );
    });
}

#[derive(Event)]
struct NetworkEvent(ServerMessage);

struct LobbyServerConn {
    conn: Connection,
    recv: mpsc::Receiver<ServerMessage>,
}
