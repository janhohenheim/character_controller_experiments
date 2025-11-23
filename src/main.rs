use avian3d::prelude::*;
use bevy::{
    ecs::{lifecycle::HookContext, world::DeferredWorld},
    gltf::GltfPlugin,
    light::CascadeShadowConfigBuilder,
    log::{LogPlugin, tracing_subscriber::field::MakeExt},
    pbr::Atmosphere,
    prelude::*,
    scene::SceneInstanceReady,
    window::WindowResolution,
};
use bevy_enhanced_input::prelude::*;
use bevy_mod_mipmap_generator::{MipmapGeneratorPlugin, generate_mipmaps};
use bevy_trenchbroom::prelude::*;
use bevy_trenchbroom_avian::AvianPhysicsBackend;

use crate::{
    character_controller::{
        CharacterController, CharacterControllerCameraOf, CharacterControllerState,
    },
    user_input::{PlayerInput, Reset},
};

mod camera;
mod character_controller;
mod debug;
mod props;
mod user_input;

fn main() -> AppExit {
    App::new()
        .add_plugins((
            DefaultPlugins
                .set(GltfPlugin {
                    use_model_forward_direction: true,
                    ..default()
                })
                .set(LogPlugin {
                    filter: format!(
                        concat!(
                            "{default},",
                            "symphonia_bundle_mp3::demuxer=warn,",
                            "symphonia_format_caf::demuxer=warn,",
                            "symphonia_format_isompf4::demuxer=warn,",
                            "symphonia_format_mkv::demuxer=warn,",
                            "symphonia_format_ogg::demuxer=warn,",
                            "symphonia_format_riff::demuxer=warn,",
                            "symphonia_format_wav::demuxer=warn,",
                            "bevy_trenchbroom::physics=off,",
                            "calloop::loop_logic=error,",
                        ),
                        default = bevy::log::DEFAULT_FILTER
                    ),
                    fmt_layer: |_| {
                        Some(Box::new(
                            bevy::log::tracing_subscriber::fmt::Layer::default()
                                .map_fmt_fields(MakeExt::debug_alt)
                                .with_writer(std::io::stderr),
                        ))
                    },
                    ..default()
                })
                .set(WindowPlugin {
                    primary_window: Window {
                        resolution: WindowResolution::new(1920, 1080),
                        ..default()
                    }
                    .into(),
                    ..default()
                }),
            PhysicsPlugins::default(),
            EnhancedInputPlugin,
            MipmapGeneratorPlugin,
            TrenchBroomPlugins(
                TrenchBroomConfig::new("character_controller_experiment")
                    .default_solid_spawn_hooks(|| {
                        SpawnHooks::new()
                            .convex_collider()
                            .smooth_by_default_angle()
                    }),
            ),
            TrenchBroomPhysicsPlugin::new(AvianPhysicsBackend),
        ))
        .add_plugins((
            user_input::plugin,
            character_controller::plugin,
            camera::plugin,
            debug::plugin,
        ))
        .add_systems(Startup, setup)
        .add_systems(Update, generate_mipmaps::<StandardMaterial>)
        .add_observer(reset_player)
        .run()
}

fn setup(mut commands: Commands, assets: Res<AssetServer>) {
    commands
        .spawn(SceneRoot(assets.load("playground.map#Scene")))
        .observe(tweak_materials);
    commands.spawn((
        Camera3d::default(),
        EnvironmentMapLight {
            diffuse_map: assets.load("environment_maps/voortrekker_interior_1k_diffuse.ktx2"),
            specular_map: assets.load("environment_maps/voortrekker_interior_1k_specular.ktx2"),
            intensity: 2000.0,
            ..default()
        },
        Projection::Perspective(PerspectiveProjection {
            fov: 70.0_f32.to_radians(),
            ..default()
        }),
        Atmosphere::EARTH,
    ));
    commands.spawn((
        Transform::from_xyz(0.0, 1.0, 0.0).looking_at(vec3(1.0, -2.0, -2.0), Vec3::Y),
        DirectionalLight {
            shadows_enabled: true,
            ..default()
        },
        CascadeShadowConfigBuilder {
            maximum_distance: 500.0,
            overlap_proportion: 0.5,
            ..default()
        }
        .build(),
    ));
}

#[derive(Component, Reflect, Debug)]
#[reflect(Component)]
struct Player;

#[point_class(base(Transform, Visibility))]
#[component(on_add = SpawnPlayer::on_add)]
struct SpawnPlayer;

impl SpawnPlayer {
    fn on_add(mut world: DeferredWorld, ctx: HookContext) {
        if world.is_scene_world() {
            return;
        }
        if world.try_query::<&Player>().unwrap().single(&world).is_ok() {
            return;
        }
        let Some(transform) = world.get::<Transform>(ctx.entity).copied() else {
            return;
        };
        let mut meshes = world.resource_mut::<Assets<Mesh>>();
        let mesh = meshes.add(Mesh::from(Cylinder::new(0.7, 1.8)));
        let mut materials = world.resource_mut::<Assets<StandardMaterial>>();
        let material = materials.add(StandardMaterial {
            base_color: bevy::color::palettes::tailwind::GREEN_600.into(),
            ..default()
        });
        let player = world
            .commands()
            .spawn((
                Player,
                transform,
                PlayerInput,
                CharacterController::default(),
                RigidBody::Kinematic,
                Collider::cylinder(0.7, 1.8),
            ))
            .with_children(|parent| {
                parent.spawn((Mesh3d(mesh), MeshMaterial3d(material)));
            })
            .id();
        let camera = world
            .try_query_filtered::<Entity, With<Camera3d>>()
            .unwrap()
            .single(&world)
            .unwrap();
        world
            .commands()
            .entity(camera)
            .insert(CharacterControllerCameraOf(player));
    }
}

fn reset_player(
    _fire: On<Fire<Reset>>,
    player: Single<(&mut Transform, &mut CharacterControllerState), With<Player>>,
    spawner: Single<&Transform, (With<SpawnPlayer>, Without<Player>)>,
) {
    let (mut transform, mut state) = player.into_inner();
    state.velocity.y = 0.0;
    transform.translation = spawner.translation;
}

fn tweak_materials(
    ready: On<SceneInstanceReady>,
    children: Query<&Children>,
    materials: Query<&MeshMaterial3d<StandardMaterial>>,
    mut material_assets: ResMut<Assets<StandardMaterial>>,
) {
    for mat in materials.iter_many(children.iter_descendants(ready.entity)) {
        let mat = material_assets.get_mut(mat).unwrap();
        mat.perceptual_roughness = 0.9;
    }
}
