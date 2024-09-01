use core::f32;
use std::{collections::BinaryHeap, time::Instant};

use bevy::{
    ecs::world::Command,
    gizmos::{clear_gizmo_context, gizmos::GizmoStorage},
    math::bounding::Bounded2d,
    prelude::*,
    render::{
        mesh::{Extrudable, Indices, PerimeterSegment},
        render_asset::RenderAssetUsages,
    },
    utils::{hashbrown::HashMap, HashSet},
};
use parry2d::{
    bounding_volume::BoundingVolume,
    math::{Isometry, Point, Vector},
    na::{Translation, UnitComplex},
    query::{Ray, RayCast},
    shape::{PolygonalFeature, Polyline},
};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::{actor::ActorId, GameClientConns, GameEvent};

pub const EPS: f32 = 0.0001;

#[derive(Component)]
pub struct Terrain {
    pub nodes: parry2d::shape::Polyline,
}

#[derive(Component)]
pub struct CustomTerrain;

impl Terrain {
    pub fn circle(radius: f32, vertex_count: usize) -> Self {
        Self::from_vertices((0..vertex_count).map(|i| {
            let angle = (i as f32 / vertex_count as f32) * std::f32::consts::PI * 2.0;
            Vec2::from_angle(angle) * radius
        }))
    }

    pub fn from_vertices(vertices: impl IntoIterator<Item = Vec2>) -> Self {
        let vertices = vertices.into_iter();
        let vec = vertices.map(Convert::conv).collect::<Vec<_>>();
        Self::from_points(vec)
    }

    pub fn from_points(points: Vec<Point<f32>>) -> Self {
        let len = points.len() as u32;
        let polyline = parry2d::shape::Polyline::new(
            points,
            Some(
                Iterator::chain(Iterator::map(0..len - 1, |i| [i, i + 1]), [[len - 1, 0]])
                    .collect(),
            ),
        );

        Self { nodes: polyline }
    }
}

#[derive(Debug, Resource)]
pub struct PathGraph {
    pub nodes: Vec<PathNode>,
    pub terrain: Vec<(Polyline, Isometry<f32>)>,
}

impl PathGraph {
    pub fn get_path(&self, start: Vec2, end: Vec2) -> Vec<Vec2> {
        // println!("Starting path finding");
        if self.terrain.iter().all(|(terrain, trans)| {
            let origin = Point::new(start.x, start.y);
            let vec2 = Point::new(end.x, end.y);
            let hit = terrain.intersects_ray(
                trans,
                &Ray::new(origin, (vec2 - origin).normalize()),
                (vec2 - origin).magnitude(),
            );

            !hit
        }) {
            // println!("No terrain blocking");
            return vec![end];
        }

        let mut nodes = self
            .nodes
            .iter()
            .map(|n| Node {
                id: n.id,
                pos: n.pos,
                fscore: f32::INFINITY,
                gscore: f32::INFINITY,
                neighbours: n.neighbours.clone(),
                came_from: None,
            })
            .collect::<Vec<_>>();

        let mut start_node = Node {
            id: nodes.len(),
            pos: start,
            fscore: (start - end).length(),
            gscore: 0.0,
            neighbours: vec![],
            came_from: None,
        };

        let mut end_node = Node {
            id: nodes.len() + 1,
            pos: end,
            fscore: f32::INFINITY,
            gscore: f32::INFINITY,
            neighbours: vec![],
            came_from: None,
        };

        let start_id = start_node.id;
        let end_id = end_node.id;

        for node in &mut nodes {
            let mut skip_for_start = false;
            let mut skip_for_end = false;

            let start_dir = (start - node.pos).normalize();
            let end_dir = (end - node.pos).normalize();

            let n = &self.nodes[node.id];

            let start_angle = start_dir.angle_between(n.normal).abs();
            let end_angle = end_dir.angle_between(n.normal).abs();

            if start_angle < n.min_range || start_angle > n.max_range {
                skip_for_start = true;
            }
            if end_angle < n.min_range || end_angle > n.max_range {
                skip_for_end = true;
            }

            if !skip_for_start
                && self.terrain.iter().all(|(terrain, trans)| {
                    let origin = start.conv();
                    let vec2: Point<_> = node.pos.conv();
                    let hit = terrain.cast_ray(
                        trans,
                        &Ray::new(origin, (vec2 - origin).normalize()),
                        (vec2 - origin).magnitude(),
                        true,
                    );

                    hit.is_none()
                })
            {
                start_node.neighbours.push(node.id);
                node.neighbours.push(start_node.id);
            }
            if !skip_for_end
                && self.terrain.iter().all(|(terrain, trans)| {
                    let origin = end.conv();
                    let vec2: Point<_> = node.pos.conv();
                    let hit = terrain.cast_ray(
                        trans,
                        &Ray::new(origin, (vec2 - origin).normalize()),
                        (vec2 - origin).magnitude(),
                        true,
                    );

                    hit.is_none()
                })
            {
                end_node.neighbours.push(node.id);
                node.neighbours.push(end_node.id);
            }
        }

        nodes.push(start_node);
        nodes.push(end_node);

        struct Node {
            id: usize,
            pos: Vec2,
            fscore: f32,
            gscore: f32,
            neighbours: Vec<usize>,
            came_from: Option<usize>,
        }

        impl PartialEq for Node {
            fn eq(&self, other: &Self) -> bool {
                self.id == other.id && self.fscore == other.fscore
            }
        }

        impl Eq for Node {}

        impl PartialOrd for Node {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                (-self.fscore).partial_cmp(&-other.fscore)
            }
        }

        impl Ord for Node {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                self.partial_cmp(other).unwrap()
            }
        }

        let mut open = HashSet::from([start_id]);

        // println!("Find {} -> {}", start_id, end_id);

        while !open.is_empty() {
            let current = open
                .iter()
                .fold(None, |min, x| match min {
                    None => Some(x),
                    Some(y) => Some(if nodes[*x].fscore < nodes[*y].fscore {
                        x
                    } else {
                        y
                    }),
                })
                .copied()
                .unwrap();
            open.remove(&current);
            // println!("Current: {}", current);
            let current = &nodes[current];
            if current.id == end_id {
                let mut path = vec![end];
                let mut current = current;
                loop {
                    if current.id == start_id {
                        // println!("Reached start");
                        break;
                    }
                    current = &nodes[current.came_from.unwrap()];
                    path.push(current.pos);
                }
                // while let Some(node) = current.came_from {
                //     current = &nodes[node];
                //     path.push(current.pos);
                // }
                path.reverse();
                return path;
            }

            let gscore = current.gscore;
            let current_pos = current.pos;
            let current_id = current.id;
            for neighbour in current.neighbours.clone() {
                // println!("Examining neighbour {}", neighbour);
                let neighbour_node = &nodes[neighbour];
                let tentative_gscore = gscore + (current_pos - neighbour_node.pos).length();
                if tentative_gscore < neighbour_node.gscore {
                    let neigh_vec2 = neighbour_node.pos;
                    nodes[neighbour].gscore = tentative_gscore;
                    nodes[neighbour].fscore = tentative_gscore + (neigh_vec2 - end).length();
                    nodes[neighbour].came_from = Some(current_id);
                    open.insert(neighbour);
                }
            }
        }

        vec![]
    }
}

#[derive(Debug)]
pub struct PathNode {
    pub id: usize,
    pub pos: Vec2,
    pub neighbours: Vec<usize>,
    pub normal: Vec2,
    pub min_range: f32,
    pub max_range: f32,
}

pub fn keep_path_graph_up_to_date(
    x: Query<
        (),
        (
            With<Terrain>,
            With<Transform>,
            Or<(Changed<Terrain>, Changed<Transform>)>,
        ),
    >,
    q: Query<(&Terrain, &Transform)>,
    mut path_graph: ResMut<PathGraph>,
) {
    if !x.is_empty() {
        *path_graph = create_path_graph(q.iter());
    }
}

fn extend_polyline(polyline: Polyline, amount: f32) -> Polyline {
    let angle_limit = 30.0f32.to_radians();

    let mut vertices = vec![];
    for i in 0..polyline.num_segments() {
        let first = polyline.segment(i as _);
        let second = polyline.segment(((i + 1) % polyline.num_segments()) as _);
        let d1 = first.direction().unwrap();
        let d2 = second.direction().unwrap();
        let d1 = d1.conv();
        let d2 = d2.conv();

        let n1 = (-Vec2::Y).rotate(d1);
        let n2 = (-Vec2::Y).rotate(d2);

        // println!("{} -> {}, {} -> {}", d1, n1, d2, n2);

        let v = n1.angle_between(n2);
        // println!("Angle: {}", v.to_degrees());

        if v > angle_limit {
            // println!("Angle {v} is > limit {angle_limit}");
            let vert_count = (v / angle_limit).ceil() as usize;
            // println!("Should be broken into {vert_count} vertices");
            let per_vertex_angle = v / vert_count as f32;
            // println!("Each having a {per_vertex_angle} angle");

            for i in 0..vert_count {
                let dir = Vec2::from_angle(per_vertex_angle * i as f32).rotate(n1);
                // println!("{dir}");
                let v = per_vertex_angle / 2.0;
                // tan v = x
                let x = v.tan();
                let dir = dir + Vec2::Y.rotate(dir) * x;
                let p = Vec2::new(first.b.x, first.b.y) + dir * amount;
                vertices.push(p.conv());
            }
        } else {
            let hv = v / 2.0;
            let d = hv.tan() * amount;

            let p = Vec2::new(first.b.x, first.b.y) + d1 * d + n1 * amount;
            // println!("{d} -- {p}");
            vertices.push(p.conv());
        }
    }

    let len = vertices.len() as u32;
    Polyline::new(
        vertices,
        Some(Iterator::chain(Iterator::map(0..len - 1, |i| [i, i + 1]), [[len - 1, 0]]).collect()),
    )
}

pub fn create_path_graph<'a, I: Iterator<Item = (&'a Terrain, &'a Transform)>>(
    terrain: I,
) -> PathGraph {
    let mut nodes = vec![];
    let mut terrains = vec![];

    let start = Instant::now();

    let terrain = terrain
        .map(|(terrain, transform)| {
            (
                extend_polyline(terrain.nodes.clone(), 0.5),
                transform.isometry(),
            )
        })
        .collect::<Vec<_>>();

    let mut index = 0;

    for (terrain, transform) in &terrain {
        let mut node_count = 0;
        for i in 0..terrain.num_segments() {
            let first = terrain.segment(i as _);
            let second = terrain.segment(((i + 1) % terrain.num_segments()) as _);
            let d1 = first.direction().unwrap();
            let d2 = second.direction().unwrap();
            let d1 = Vec2::new(d1.x, d1.y);
            let d2 = Vec2::new(d2.x, d2.y);
            if d1.angle_between(d2) > 0.0 {
                let first_normal = (-Vec2::Y).rotate(d1);
                let normal = (first_normal + (-Vec2::Y).rotate(d2)).normalize();
                let pos =
                    transform * first.b + parry2d::math::Vector::new(normal.x, normal.y) * EPS;
                let min_range = d1.angle_between(normal).abs() - 0.05;
                let max_range = d1.angle_between(-normal).abs() + 0.05;
                nodes.push(PathNode {
                    id: nodes.len(),
                    pos: pos.conv(),
                    neighbours: vec![],
                    normal,
                    min_range,
                    max_range,
                });
                node_count += 1;
            }
        }
        terrains.push(index..index + node_count);
        index += node_count;
    }

    let mut connections: HashSet<(usize, usize)> = HashSet::new();

    let mut iter = PairsIter::new(&mut nodes);
    // while let Some((node1, node2)) = iter.next() {
    for (i, node1) in nodes.iter().enumerate() {
        for (j, node2) in nodes.iter().take(i).enumerate() {
            let dir = (node2.pos - node1.pos).normalize();

            let node1_angle = dir.angle_between(node1.normal).abs();
            let node2_angle = dir.angle_between(node2.normal).abs();

            if node1_angle < node1.min_range
                || node2_angle < node2.min_range
                || node1_angle > node1.max_range
                || node2_angle > node2.max_range
            {
                continue;
            }

            if terrain.iter().all(|(terrain, trans)| {
                let hit = terrain.cast_ray(
                    trans,
                    &Ray::new(node1.pos.conv(), dir.conv()),
                    (node2.pos - node1.pos).length(),
                    true,
                );

                hit.is_none()
            }) {
                connections.insert((j, i));
                // node1.neighbours.push(node2.id);
                // node2.neighbours.push(node1.id);
            }
        }
    }
    let edge_count = connections.len();
    for (a, b) in connections {
        nodes[a].neighbours.push(b);
        nodes[b].neighbours.push(a);
    }

    let time = Instant::now().duration_since(start).as_secs_f32();

    println!(
        "Calculated path graph in {time} seconds with {} vertices and {} edges",
        nodes.len(),
        edge_count
    );
    PathGraph { nodes, terrain }
}

struct PairsIter<'a, T> {
    vec: &'a mut Vec<T>,
    i: usize,
    j: usize,
}

impl<'a, T> PairsIter<'a, T> {
    fn new(vec: &'a mut Vec<T>) -> Self {
        Self { vec, i: 1, j: 0 }
    }

    fn next<'b>(&'b mut self) -> Option<(&'b mut T, &'b mut T)> {
        if self.j >= self.i {
            self.i += 1;
            self.j = 0;
        }
        if self.i >= self.vec.len() {
            return None;
        }

        let [item_1, item_2] = self.vec.get_many_mut([self.i, self.j]).unwrap();

        self.j += 1;

        Some((item_1, item_2))
    }
}

pub fn draw_path_graph(path_graph: Res<PathGraph>, mut gizmos: Gizmos) {
    let mut visited = HashSet::new();

    for node in &path_graph.nodes {
        visited.insert(node.id);
        for neighbour in &node.neighbours {
            if visited.contains(neighbour) {
                continue;
            }

            let a = node.pos;
            let b = path_graph.nodes[*neighbour].pos;

            gizmos.line(a.extend(0.1), b.extend(0.1), Color::srgb(0.0, 0.0, 1.0));
        }
    }

    for terrain in &path_graph.terrain {
        for segment in terrain.0.segments() {
            let segment = segment.transformed(&terrain.1);
            let a: Vec2 = segment.a.conv();
            let b: Vec2 = segment.b.conv();

            gizmos.line(
                a.extend(0.105),
                b.extend(0.105),
                Color::srgba(1.0, 0.0, 0.0, 0.5),
            );
        }
    }
}

pub trait Convert<T> {
    fn conv(self) -> T;
}

impl Convert<Vector<f32>> for Vec2 {
    fn conv(self) -> Vector<f32> {
        Vector::new(self.x, self.y)
    }
}

impl Convert<Point<f32>> for Vec2 {
    fn conv(self) -> Point<f32> {
        Point::new(self.x, self.y)
    }
}

impl Convert<Vec2> for Vector<f32> {
    fn conv(self) -> Vec2 {
        Vec2::new(self.x, self.y)
    }
}

impl Convert<Vec2> for Point<f32> {
    fn conv(self) -> Vec2 {
        Vec2::new(self.x, self.y)
    }
}

pub trait ToIsometry {
    fn isometry(&self) -> Isometry<f32>;
}

impl ToIsometry for Transform {
    fn isometry(&self) -> Isometry<f32> {
        Isometry::new(
            self.translation.xy().conv(),
            self.rotation.to_euler(EulerRot::XYZ).2,
        )
    }
}

pub struct M<T>(pub T);

impl<T: Primitive2d> Primitive2d for M<T> {}

impl Meshable for M<BoxedPolyline2d> {
    type Output = PolyMeshBuilder;

    fn mesh(&self) -> Self::Output {
        PolyMeshBuilder {
            shape: self.0.clone(),
        }
    }
}

impl Extrudable for PolyMeshBuilder {
    fn perimeter(&self) -> Vec<bevy::render::mesh::PerimeterSegment> {
        vec![PerimeterSegment::Flat {
            indices: (0..self.shape.vertices.len() as u32).chain([0]).collect(),
        }]
    }
}

pub struct PolyMeshBuilder {
    shape: BoxedPolyline2d,
}

impl MeshBuilder for PolyMeshBuilder {
    fn build(&self) -> Mesh {
        let trimesh = parry2d::shape::TriMesh::from_polygon(
            self.shape
                .vertices
                .iter()
                .copied()
                .map(Convert::conv)
                .collect(),
        )
        .unwrap();

        let verts = trimesh
            .vertices()
            .iter()
            .copied()
            .map(|p| Vec3::new(p.x, p.y, 0.0))
            .collect::<Vec<_>>();

        Mesh::new(
            bevy::render::mesh::PrimitiveTopology::TriangleList,
            RenderAssetUsages::all(),
        )
        // Assign normals (everything points outwards)
        .with_inserted_attribute(
            Mesh::ATTRIBUTE_NORMAL,
            verts.iter().map(|_| [0.0, 0.0, 1.0]).collect::<Vec<_>>(),
        )
        .with_inserted_attribute(Mesh::ATTRIBUTE_POSITION, verts)
        .with_inserted_indices(Indices::U32(trimesh.indices().as_flattened().to_vec()))
    }
}

pub struct SaveTerrain;

impl Command for SaveTerrain {
    fn apply(self, world: &mut World) {
        let path = "./terrain.json";

        let data = TerrainSaveData {
            terrain_objects: world
                .query_filtered::<(&Terrain, &Transform), With<CustomTerrain>>()
                .iter(world)
                .map(|(terrain, trans)| {
                    terrain
                        .nodes
                        .vertices()
                        .iter()
                        .copied()
                        .map(|node| node.conv() + trans.translation.xy())
                        .collect()
                })
                .collect(),
        };

        std::fs::write(path, serde_json::to_string_pretty(&data).unwrap()).unwrap();
    }
}

pub struct LoadTerrain;

impl Command for LoadTerrain {
    fn apply(self, world: &mut World) {
        let path = "./terrain.json";

        for (e, actor) in world
            .query_filtered::<(Entity, &ActorId), With<CustomTerrain>>()
            .iter(world)
            .map(|x| (x.0, *x.1))
            .collect::<Vec<_>>()
        {
            world.entity_mut(e).despawn_recursive();
            world
                .resource_mut::<GameClientConns>()
                .broadcast(&GameEvent::DeleteActor(actor));
        }

        let data: Result<TerrainSaveData, anyhow::Error> =
            try { serde_json::from_slice(&std::fs::read(path)?)? };

        let data = data.unwrap_or(TerrainSaveData {
            terrain_objects: vec![],
        });

        for mut terrain in data.terrain_objects {
            let mut center = Vec2::ZERO;
            for vert in &terrain {
                center += *vert;
            }
            center /= terrain.len() as f32;
            for vert in &mut terrain {
                *vert -= center;
            }

            let actor_id = ActorId(Uuid::new_v4());

            world.spawn((
                CustomTerrain,
                actor_id,
                TransformBundle::from_transform(Transform::from_translation(center.extend(0.0))),
                Terrain::from_vertices(terrain.iter().copied()),
            ));

            world
                .resource_mut::<GameClientConns>()
                .broadcast(&GameEvent::CreateTerrain(actor_id, terrain, center));
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct TerrainSaveData {
    terrain_objects: Vec<Vec<Vec2>>,
}
