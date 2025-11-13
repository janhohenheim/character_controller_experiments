use avian3d::prelude::*;
use bevy::{
    ecs::{
        lifecycle::HookContext, relationship::RelationshipSourceCollection, world::DeferredWorld,
    },
    prelude::*,
};

mod fixed_update_util;
mod input;
mod quake_1;

pub(crate) use input::{AccumulatedInput, Jump, Movement};

pub(super) fn plugin(app: &mut App) {
    app.add_plugins((fixed_update_util::plugin, input::plugin, quake_1::plugin))
        .configure_sets(
            FixedPostUpdate,
            CharacterControllerSystems::ApplyMovement.in_set(PhysicsSystems::First),
        );
}

#[derive(Component, Clone, Reflect, Debug)]
#[reflect(Component)]
#[require(
    AccumulatedInput,
    CharacterControllerState,
    RigidBody = RigidBody::Kinematic,
    Collider = panic!("CharacterController requires a root Collider") as Collider
)]
#[component(on_add=CharacterController::on_add)]
pub(crate) struct CharacterController {
    pub(crate) speed: Vec2,
    pub(crate) air_speed: f32,
    pub(crate) acceleration: f32,
    pub(crate) max_speed: f32,
    pub(crate) gravity: f32,
    pub(crate) friction_hz: f32,
    pub(crate) stop_speed: f32,
    pub(crate) filter: SpatialQueryFilter,
}

impl Default for CharacterController {
    fn default() -> Self {
        Self {
            speed: vec2(10., 9.),
            air_speed: 0.7,
            acceleration: 0.25,
            max_speed: 8.0,
            gravity: 20.,
            friction_hz: 4.0,
            stop_speed: 2.5,
            filter: SpatialQueryFilter::default(),
        }
    }
}

impl CharacterController {
    pub fn on_add(mut world: DeferredWorld, ctx: HookContext) {
        let Some(mut kcc) = world.get_mut::<Self>(ctx.entity) else {
            return;
        };
        kcc.filter.excluded_entities.add(ctx.entity);
    }
}

#[derive(Component, Clone, Copy, Reflect, Default, Debug)]
#[reflect(Component)]
pub(crate) struct CharacterControllerState {
    pub(crate) grounded: Option<Entity>,
}

#[derive(SystemSet, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum CharacterControllerSystems {
    ApplyMovement,
}
