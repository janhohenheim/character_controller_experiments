use avian3d::prelude::*;
use bevy::prelude::*;

mod custom;
mod fixed_update_utils;
mod input;
#[allow(dead_code)]
mod quake_1;
#[allow(dead_code)]
mod quake_3;

use custom as kcc;
pub(crate) use input::*;
pub(crate) use kcc::*;

pub(super) fn plugin(app: &mut App) {
    app.add_plugins((input::plugin, kcc::plugin, fixed_update_utils::plugin))
        .add_systems(
            RunFixedMainLoop,
            sync_camera_transform.after(TransformEasingSystems::UpdateEasingTick),
        )
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
#[relationship(relationship_target = CharacterControllerCamera)]
pub(crate) struct CharacterControllerCameraOf(pub(crate) Entity);

#[derive(Component, Clone, Copy)]
#[relationship_target(relationship = CharacterControllerCameraOf)]
pub(crate) struct CharacterControllerCamera(Entity);

fn sync_camera_transform(
    mut cameras: Query<
        (&mut Transform, &CharacterControllerCameraOf),
        (Without<CharacterControllerState>,),
    >,
    kccs: Query<(&Transform, &CharacterController, &CharacterControllerState)>,
) {
    // TODO: DIY TransformHelper to use current global transform.
    // Can't use GlobalTransform directly: outdated -> jitter
    // Can't use TransformHelper directly: access conflict with &mut Transform
    for (mut camera_transform, camera_of) in cameras.iter_mut() {
        if let Ok((kcc_transform, cfg, state)) = kccs.get(camera_of.0) {
            let height = state
                // changing the collider does not change the transform, so to get the correct position for the feet,
                // we need to use the collider we spawned with.
                .standing_collider
                .aabb(Vec3::default(), Rotation::default())
                .size()
                .y;
            let view_height = if state.crouching {
                cfg.crouch_view_height
            } else {
                cfg.standing_view_height
            };
            camera_transform.translation =
                kcc_transform.translation + Vec3::Y * (-height / 2.0 + view_height);
        }
    }
}
