use avian3d::prelude::*;
use bevy::{
    ecs::{lifecycle::HookContext, world::DeferredWorld},
    gltf::GltfPlugin,
    prelude::*,
};
use bevy_enhanced_input::prelude::*;
use bevy_trenchbroom::prelude::*;
use bevy_trenchbroom_avian::AvianPhysicsBackend;

use crate::{character_controller::CharacterController, user_input::PlayerInput};

mod character_controller;
mod user_input;

fn main() -> AppExit {
    App::new()
        .add_plugins((
            DefaultPlugins.set(GltfPlugin {
                use_model_forward_direction: true,
                ..default()
            }),
            PhysicsPlugins::default(),
            EnhancedInputPlugin,
            TrenchBroomPlugins(TrenchBroomConfig::new("character_controller_experiment")),
            TrenchBroomPhysicsPlugin::new(AvianPhysicsBackend),
        ))
        .add_plugins((user_input::plugin, character_controller::plugin))
        .add_systems(Startup, setup)
        .add_systems(Update, print_transform)
        .run()
}

fn setup(mut commands: Commands, assets: Res<AssetServer>) {
    commands.spawn(SceneRoot(assets.load("playground.map#Scene")));
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
            Camera3d::default(),
            PlayerInput,
            CharacterController::default(),
            RigidBody::Kinematic,
            Collider::capsule(0.25, 1.3),
        ));
    }
}

fn print_transform(transform: Single<&Transform, With<CharacterController>>) {
    println!("Position: {}", transform.translation);
}
