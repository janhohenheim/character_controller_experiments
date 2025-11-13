use avian3d::prelude::*;
use bevy::{
    ecs::{lifecycle::HookContext, world::DeferredWorld},
    gltf::GltfPlugin,
    prelude::*,
};
use bevy_enhanced_input::prelude::*;
use bevy_trenchbroom::prelude::*;
use bevy_trenchbroom_avian::AvianPhysicsBackend;

mod character_controller;
mod fixed_update_util;
mod input;

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
        .add_plugins((
            fixed_update_util::plugin,
            input::plugin,
            character_controller::plugin,
        ))
        .add_systems(Startup, setup)
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
        world
            .commands()
            .entity(ctx.entity)
            .insert((Camera3d::default(),));
    }
}
