use std::{
    collections::HashMap,
    fmt::Display,
    io::{self, Read, Write},
    net::{IpAddr, SocketAddr, TcpListener, TcpStream},
    sync::mpsc,
};

use anyhow::Ok;
use serde::{Deserialize, Serialize};
use uuid::Uuid;
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PlayerId(pub Uuid);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct LobbyId(pub Uuid);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum Team {
    Red,
    Blue,
}

impl Team {
    pub fn all() -> [Team; 2] {
        [Team::Red, Team::Blue]
    }
}

impl Display for Team {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Team::Red => "Red",
            Team::Blue => "Blue",
        })
    }
}

#[derive(Debug)]
pub struct Connection {
    pub conn: TcpStream,
}

impl Connection {
    pub fn read<T: for<'de> Deserialize<'de> + std::fmt::Debug>(&mut self) -> anyhow::Result<T> {
        let mut len = [0; std::mem::size_of::<u32>()];
        self.conn.read_exact(&mut len)?;
        let len = u32::from_be_bytes(len) as usize;
        let mut buffer = vec![0; len];
        self.conn.read_exact(&mut buffer)?;
        let t = serde_json::from_slice(&buffer)?;
        println!("RECV {t:#?}");
        Ok(t)
    }

    pub fn write<T: Serialize + std::fmt::Debug>(&mut self, val: &T) -> anyhow::Result<()> {
        let bytes = serde_json::to_string(&val)?;
        let len = bytes.len();
        self.conn.write_all(&u32::to_be_bytes(len as _))?;
        self.conn.write_all(bytes.as_bytes())?;
        println!("SEND {val:#?}");
        Ok(())
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub enum ClientMessage {
    SetName(String),
    CreateNewLobby,
    JoinLobby(LobbyId),
    LeaveLobby,
    GetLobbyInfo(LobbyId),
    GetLobbyList,
    SwitchTeam(Team),
    GetPlayerInfo(PlayerId),
    GetMyId,
    StartGame,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ServerMessage {
    LobbyJoined(LobbyId),
    LobbyLeft,
    LobbyInfo(Lobby),
    LobbyList(Vec<(LobbyId, usize)>),
    PlayerNameUpdate(PlayerId, String),
    PlayerJoinedLobby(PlayerId),
    PlayerLeftLobby(PlayerId),
    PlayerSwitchedTeam(PlayerId),
    PlayerInfo { player: Player, team: Option<Team> },
    YourPlayerId(PlayerId),
    JoinGame(SocketAddr, PlayerToken),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Lobby {
    pub id: LobbyId,
    pub players: HashMap<Team, Vec<PlayerId>>,
    pub leader: PlayerId,
}

impl Lobby {
    pub fn new() -> Self {
        Self {
            id: LobbyId(Uuid::new_v4()),
            players: [(Team::Red, vec![]), (Team::Blue, vec![])].into(),
            leader: PlayerId(Uuid::nil()),
        }
    }

    pub fn switch_leader(&mut self, new_leader: PlayerId) {
        self.leader = new_leader;
    }

    pub fn add_player(&mut self, player: PlayerId) {
        let team = self
            .players_sorted()
            .into_iter()
            .min_by_key(|x| x.1.len())
            .map_or(Team::Red, |x| x.0);
        let player_id = player;
        self.players.get_mut(&team).unwrap().push(player);

        if self.leader.0.is_nil() {
            self.leader = player_id;
        }
    }

    fn players_sorted(&self) -> Vec<(Team, &[PlayerId])> {
        let mut players = self
            .players
            .iter()
            .map(|(a, b)| (*a, b.as_slice()))
            .collect::<Vec<_>>();
        players.sort_by_key(|x| x.0);
        players
    }

    pub fn remove_player(&mut self, player_id: PlayerId) {
        for (_, players) in self.players.iter_mut() {
            if let Some(i) = players.iter().position(|p| *p == player_id) {
                players.remove(i);
                break;
            }
        }

        if self.leader == player_id {
            self.leader = self
                .players_sorted()
                .into_iter()
                .find_map(|x| x.1.first().copied())
                .unwrap_or(PlayerId(Uuid::nil()));
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Player {
    pub id: PlayerId,
    pub name: String,
    pub in_lobby: Option<LobbyId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PlayerToken(pub Uuid);

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ToGameServer {
    Prepare {
        players: Vec<(Team, Player, PlayerToken)>,
    },
}
