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
    /// The maximum speed the character can reach.
    ///
    /// Default: 8 m/s
    pub(crate) max_speed: f32,
    /// The gravity acceleration applied to the character.
    ///
    /// Default: 20 m/s²
    pub(crate) gravity: f32,
    pub(crate) friction_hz: f32,
    pub(crate) stop_speed: f32,
    /// The cosine of the maximum slope angle the character can climb.
    ///
    /// Default: 0.7 (corresponds to 45.57 degrees)
    pub(crate) max_slope_cosine: f32,
    /// If the y component of the velocity is above this value, the character is considered airborne regardless of ground contact.
    /// This allows sliding up slopes if fast enough.
    ///
    /// Default: 4.5 m/s
    pub(crate) airborne_speed: f32,
    /// Implicitly always excludes the character's own entity.
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
            // 45.57 degrees
            max_slope_cosine: 0.7,
            airborne_speed: 4.5,
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
