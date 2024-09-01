#![feature(new_range_api)]

use core::range::RangeInclusive;
use std::{
    collections::HashMap,
    error::Error,
    fmt::{Display, Write},
    io::{self, Read, Write as _},
    net::{IpAddr, SocketAddr, TcpListener, TcpStream},
    str::FromStr,
    sync::mpsc,
};

use serde::{Deserialize, Serialize};
use uuid::Uuid;
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PlayerId(pub Uuid);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct LobbyId(pub Uuid);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Team(pub usize);

impl Display for Team {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self.0 {
            0 => "Red",
            1 => "Blue",
            other => return write!(f, "Team {}", other + 1),
        })
    }
}

#[derive(Debug)]
pub struct Connection {
    pub conn: TcpStream,
}

impl Connection {
    pub fn read<T: for<'de> Deserialize<'de> + std::fmt::Debug>(&mut self) -> anyhow::Result<T> {
        // let thread = std::thread::current();
        // let thread_name = thread.name().unwrap_or("<unnamed>");
        // let thread_id = thread.id();
        // let id = format!("{thread_name}:{thread_id:?}");
        // println!("{id} -- Starting reading");
        let mut len = [0; std::mem::size_of::<u32>()];
        self.conn.read_exact(&mut len)?;
        let len = u32::from_be_bytes(len) as usize;
        // println!("{id} -- Read length {len}");
        let mut buffer = vec![0; len];
        self.conn.read_exact(&mut buffer)?;
        // println!("{id} -- Read bytes {:?}", String::from_utf8_lossy(&buffer));
        let t = serde_json::from_slice(&buffer)?;
        // println!("{id} -- Read data {t:#?}");
        Ok(t)
    }

    pub fn write<T: Serialize + std::fmt::Debug>(&mut self, val: &T) -> anyhow::Result<()> {
        // let thread = std::thread::current();
        // let thread_name = thread.name().unwrap_or("<unnamed>");
        // let thread_id = thread.id();
        // let id = format!("{thread_name}:{thread_id:?}");
        // println!("{id} -- Starting writing");
        let bytes = serde_json::to_string(&val)?;
        let len = bytes.len();
        self.conn.write_all(&u32::to_be_bytes(len as _))?;
        // println!("{id} -- Wrote length {len}");
        self.conn.write_all(bytes.as_bytes())?;
        // println!("{id} -- Wrote bytes {:?}", bytes);
        // println!("{id} -- Wrote data {val:#?}");
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
pub struct GameSettings {
    pub team_count: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Lobby {
    pub id: LobbyId,
    pub settings: GameSettings,
    pub players: HashMap<Team, Vec<PlayerId>>,
    pub leader: PlayerId,
}

impl Lobby {
    pub fn new() -> Self {
        Self {
            id: LobbyId(Uuid::new_v4()),
            settings: GameSettings { team_count: 2 },
            players: HashMap::new(),
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
            .map_or(Team(0), |x| x.0);
        let player_id = player;
        self.players.entry(team).or_default().push(player);

        if self.leader.0.is_nil() {
            self.leader = player_id;
        }
    }

    fn players_sorted(&self) -> Vec<(Team, &[PlayerId])> {
        (0..self.settings.team_count)
            .map(|x| {
                (
                    Team(x),
                    self.players
                        .get(&Team(x))
                        .map(Vec::as_slice)
                        .unwrap_or_default(),
                )
            })
            .collect::<Vec<_>>()
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PortRange {
    pub first: u16,
    pub last: u16,
}

impl PortRange {
    pub fn new(first: u16, last: u16) -> Self {
        Self { first, last }
    }

    pub fn single(port: u16) -> Self {
        Self::new(port, port)
    }

    pub fn range(&self) -> RangeInclusive<u16> {
        RangeInclusive {
            start: self.first,
            end: self.last,
        }
    }
}

#[derive(Debug)]
pub enum PortRangeParseError {
    CannotSplit,
    Other(Box<dyn Error + Send + Sync>),
}

impl Display for PortRangeParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PortRangeParseError::CannotSplit => f.write_str("Invalid port range format"),
            PortRangeParseError::Other(x) => x.fmt(f),
        }
    }
}

impl Error for PortRangeParseError {}

impl FromStr for PortRange {
    type Err = PortRangeParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some((first, last)) = s.split_once('-') {
            Ok(Self::new(
                first
                    .parse()
                    .map_err(|e| PortRangeParseError::Other(Box::new(e)))?,
                last.parse()
                    .map_err(|e| PortRangeParseError::Other(Box::new(e)))?,
            ))
        } else {
            Ok(Self::single(
                s.parse()
                    .map_err(|e| PortRangeParseError::Other(Box::new(e)))?,
            ))
        }
    }
}

impl Display for PortRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.first.fmt(f)?;
        f.write_char('-');
        self.last.fmt(f)?;
        Ok(())
    }
}
