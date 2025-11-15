use avian3d::prelude::*;
use bevy::{
    ecs::{lifecycle::HookContext, world::DeferredWorld},
    gltf::GltfPlugin,
    log::{LogPlugin, tracing_subscriber::field::MakeExt},
    prelude::*,
};
use bevy_enhanced_input::prelude::*;
use bevy_trenchbroom::prelude::*;
use bevy_trenchbroom_avian::AvianPhysicsBackend;

use crate::{
    character_controller::{CharacterController, KccRotationOf},
    user_input::PlayerInput,
};

mod camera;
mod character_controller;
mod debug;
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
                }),
            PhysicsPlugins::default(),
            EnhancedInputPlugin,
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
        .run()
}

fn setup(mut commands: Commands, assets: Res<AssetServer>) {
    commands.spawn(SceneRoot(assets.load("playground.map#Scene")));
    commands.spawn((
        Camera3d::default(),
        EnvironmentMapLight {
            diffuse_map: assets.load("environment_maps/voortrekker_interior_1k_diffuse.ktx2"),
            specular_map: assets.load("environment_maps/voortrekker_interior_1k_specular.ktx2"),
            intensity: 2000.0,
            ..default()
        },
    ));
}

#[point_class(base(Transform, Visibility))]
#[component(on_add = Player::on_add)]
struct Player;

impl Player {
    fn on_add(mut world: DeferredWorld, ctx: HookContext) {
        if world.is_scene_world() {
            return;
        }
        world.commands().entity(ctx.entity).insert((
            PlayerInput,
            CharacterController::default(),
            RigidBody::Kinematic,
            TransformInterpolation,
            Collider::cylinder(0.25, 1.8),
        ));
        let camera = world
            .try_query_filtered::<Entity, With<Camera3d>>()
            .unwrap()
            .single(&*world)
            .unwrap();
        world
            .commands()
            .entity(camera)
            .insert(KccRotationOf(ctx.entity));
    }
}
