use avian3d::prelude::*;
use bevy::prelude::*;

mod fixed_update_util;
mod input;
#[allow(dead_code)]
mod quake_1;
#[allow(dead_code)]
mod quake_3;

pub(crate) use input::*;
pub(crate) use kcc::*;
use quake_3 as kcc;

pub(super) fn plugin(app: &mut App) {
    app.add_plugins((fixed_update_util::plugin, input::plugin, kcc::plugin))
        .configure_sets(
            FixedPostUpdate,
            CharacterControllerSystems::ApplyMovement.in_set(PhysicsSystems::First),
        );
}

#[derive(SystemSet, Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum CharacterControllerSystems {
    ApplyMovement,
}

#[derive(Component, Clone, Copy)]
#[relationship(relationship_target = KccRotation)]
pub(crate) struct KccRotationOf(pub(crate) Entity);

#[derive(Component, Clone, Copy)]
#[relationship_target(relationship = KccRotationOf)]
pub(crate) struct KccRotation(Entity);
