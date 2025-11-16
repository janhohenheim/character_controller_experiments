use std::sync::Arc;

use avian3d::{
    parry::{
        query::ShapeCastHit,
        shape::{Capsule, SharedShape},
    },
    prelude::*,
};
use bevy::{
    ecs::{
        lifecycle::HookContext, relationship::RelationshipSourceCollection as _,
        world::DeferredWorld,
    },
    prelude::*,
};

use crate::character_controller::{
    CharacterControllerCamera, CharacterControllerCameraOf, CharacterControllerSystems,
    input::AccumulatedInput,
};

#[derive(Component, Clone, Reflect, Debug)]
#[reflect(Component)]
#[require(
    AccumulatedInput,
    CharacterControllerState,
    RigidBody = RigidBody::Kinematic,
    Collider = Collider::cylinder(0.7, 1.8)
)]
#[component(on_add=CharacterController::on_add)]
pub(crate) struct CharacterController {
    pub(crate) crouch_height: f32,
    pub(crate) filter: SpatialQueryFilter,
    pub(crate) skin_width: f32,
    pub(crate) standing_view_height: f32,
    pub(crate) crouch_view_height: f32,
    pub(crate) ground_distance: f32,
    pub(crate) jump_detection_speed: f32,
    pub(crate) min_walk_cos: f32,
    pub(crate) stop_speed: f32,
    pub(crate) friction_hz: f32,
    pub(crate) speed: Vec2,
    pub(crate) air_acceleration_hz: f32,
}

impl Default for CharacterController {
    fn default() -> Self {
        Self {
            crouch_height: 1.3,
            filter: SpatialQueryFilter::default(),
            skin_width: 0.01,
            standing_view_height: 1.7,
            crouch_view_height: 1.2,
            ground_distance: 0.015,
            jump_detection_speed: 0.5,
            min_walk_cos: 0.7,
            stop_speed: 5.0,
            friction_hz: 6.0,
            speed: Vec2::new(7.0, 8.0),
            air_acceleration_hz: 1.0,
        }
    }
}

impl CharacterController {
    pub fn on_add(mut world: DeferredWorld, ctx: HookContext) {
        {
            let Some(mut kcc) = world.get_mut::<Self>(ctx.entity) else {
                return;
            };
            kcc.filter.excluded_entities.add(ctx.entity);
        }

        let crouch_height = {
            let Some(kcc) = world.get::<Self>(ctx.entity) else {
                return;
            };
            kcc.crouch_height
        };

        let Some(collider) = world.entity(ctx.entity).get::<Collider>().cloned() else {
            return;
        };
        let standing_aabb = collider.aabb(default(), Rotation::default());
        let standing_height = standing_aabb.max.y - standing_aabb.min.y;

        let Some(mut state) = world.get_mut::<CharacterControllerState>(ctx.entity) else {
            return;
        };
        state.standing_collider = collider.clone();

        let frac = crouch_height / standing_height;

        let mut crouching_collider = Collider::from(SharedShape(Arc::from(
            state.standing_collider.shape().clone_dyn(),
        )));

        if crouching_collider.shape().as_cylinder().is_some() {
            let capsule = crouching_collider
                .shape_mut()
                .make_mut()
                .as_capsule_mut()
                .unwrap();
            let radius = capsule.radius;
            let new_height = (crouch_height - radius).max(0.0);
            // TODO: support non-Y-aligned capsules
            *capsule = Capsule::new_y(new_height / 2.0, radius);
        } else {
            // note: well-behaved shapes like cylinders and cuboids will not actually subdivide when scaled, yay
            crouching_collider.set_scale(Vec3::Y * frac, 16);
        }
        state.crouching_collider = Collider::compound(vec![(
            Vec3::Y * (crouch_height - standing_height) / 2.0,
            Rotation::default(),
            crouching_collider,
        )])
    }
}

#[derive(Component, Clone, Reflect, Default, Debug)]
#[reflect(Component)]
pub(crate) struct CharacterControllerState {
    pub(crate) previous_transform: Transform,
    pub(crate) previous_velocity: Vec3,
    #[reflect(ignore)]
    pub(crate) standing_collider: Collider,
    #[reflect(ignore)]
    pub(crate) crouching_collider: Collider,
    pub(crate) previous_grounded: Option<ShapeHitData>,
    pub(crate) grounded: Option<ShapeHitData>,
    pub(crate) crouching: bool,
    pub(crate) ground_plane: bool,
    pub(crate) grounded_entity: Option<Entity>,
    pub(crate) walking: bool,
}

impl CharacterControllerState {
    pub(crate) fn collider(&self) -> &Collider {
        if self.crouching {
            &self.crouching_collider
        } else {
            &self.standing_collider
        }
    }
}

pub(super) fn plugin(app: &mut App) {
    app.add_systems(
        FixedPostUpdate,
        run_kcc.in_set(CharacterControllerSystems::ApplyMovement),
    )
    .add_systems(
        RunFixedMainLoop,
        sync_camera_transform.after(TransformEasingSystems::UpdateEasingTick),
    );
}

fn sync_camera_transform(
    mut cameras: Query<
        (&mut Transform, &CharacterControllerCameraOf),
        (Without<CharacterControllerState>,),
    >,
    kccs: Query<(
        &Transform,
        &CharacterController,
        &Collider,
        &CharacterControllerState,
    )>,
) {
    // TODO: DIY TransformHelper to use current global transform.
    // Can't use GlobalTransform directly: outdated -> jitter
    // Can't use TransformHelper directly: access conflict with &mut Transform
    for (mut camera_transform, camera_of) in cameras.iter_mut() {
        if let Ok((kcc_transform, cfg, collider, state)) = kccs.get(camera_of.0) {
            let height = if state.crouching {
                cfg.crouch_height
            } else {
                collider.aabb(default(), Rotation::default()).size().y
            };
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

fn run_kcc(
    world: &mut World,
    mut kccs: Local<
        QueryState<(
            Entity,
            &CharacterController,
            &CharacterControllerState,
            &AccumulatedInput,
            &LinearVelocity,
            &GlobalTransform,
            &ColliderAabb,
            Option<&CharacterControllerCamera>,
        )>,
    >,
    mut scratch: Local<Vec<(Transform, Vec3, CharacterControllerState, Ctx)>>,
) {
    let dt = world.resource::<Time>().delta_secs();
    scratch.extend(kccs.iter(world).map(
        |(entity, cfg, state, input, vel, transform, aabb, camera)| {
            let transform = transform.compute_transform();
            (
                transform,
                vel.0,
                state.clone(),
                Ctx {
                    entity,
                    cfg: cfg.clone(),
                    input: *input,
                    dt,
                    aabb: *aabb,
                    orientation: camera
                        .and_then(|e| world.entity(e.0).get::<Transform>().copied())
                        .unwrap_or(transform),
                },
            )
        },
    ));
    for (transform, velocity, state, ctx) in scratch.drain(..) {
        let entity = ctx.entity;
        let (transform, velocity, state): (Transform, Vec3, CharacterControllerState) =
            match world.run_system_cached_with(move_single, (transform, velocity, state, ctx)) {
                Ok(val) => val,
                Err(err) => {
                    error!("Error running air_move system: {}", err);
                    continue;
                }
            };
        **world
            .entity_mut(entity)
            .get_mut::<LinearVelocity>()
            .unwrap() = velocity;
        *world
            .entity_mut(entity)
            .get_mut::<CharacterControllerState>()
            .unwrap() = state;
        *world.entity_mut(entity).get_mut::<Transform>().unwrap() = transform;
    }
}

#[derive(Debug)]
struct Ctx {
    entity: Entity,
    orientation: Transform,
    cfg: CharacterController,
    input: AccumulatedInput,
    aabb: ColliderAabb,
    dt: f32,
}

#[must_use]
fn move_single(
    In((mut transform, mut velocity, mut state, ctx)): In<(
        Transform,
        Vec3,
        CharacterControllerState,
        Ctx,
    )>,
    spatial: Res<SpatialQueryPipeline>,
) -> (Transform, Vec3, CharacterControllerState) {
    state.previous_transform = transform;
    state.previous_velocity = velocity;
    // here we'd handle things like spectator, dead, noclip, etc.

    check_duck(transform, &spatial, &mut state, &ctx);

    ground_trace(transform, velocity, &spatial, &mut state, &ctx);

    if state.walking {
        walk_move();
    } else {
        air_move();
    }
    ground_trace(transform, velocity, &spatial, &mut state, &ctx);

    (transform, velocity, state)
}

fn walk_move() {}

fn air_move(
    mut transform: Transform,
    mut velocity: Vec3,
    state: &CharacterControllerState,
    ctx: &Ctx,
) -> (Transform, Vec3) {
    velocity = friction(velocity, state, ctx);

    let movement = ctx.input.last_movement.unwrap_or_default();
    let cfg_speed = ctx.cfg.speed;
    let mut wish_vel = cfg_speed.y * movement.y * ctx.orientation.forward()
        + cfg_speed.x * movement.x * ctx.orientation.right();
    wish_vel.y = 0.0;
    let (wish_dir, mut wish_speed) = Dir3::new_and_length(wish_vel).unwrap_or((Dir3::NEG_Z, 0.0));

    // not on ground, so little effect on velocity
    velocity = accelerate(
        wish_dir,
        wish_speed,
        velocity,
        ctx.cfg.air_acceleration_hz,
        ctx,
    );

    // we may have a ground plane that is very steep, even
    // though we don't have a groundentity
    // slide along the steep plane
    if state.ground_plane {
        velocity = clip_velocity(velocity, state.grounded.unwrap().normal1);
    }

    step_slide_move();

    (transform, velocity)
}

#[must_use]
fn clip_velocity(velocity: Vec3, normal: Vec3) -> Vec3 {
    const OVERCLIP: f32 = 1.001;
    let backoff = velocity.dot(normal);
    let backoff = if backoff < 0.0 {
        backoff * OVERCLIP
    } else {
        backoff / OVERCLIP
    };
    velocity - normal * backoff
}

#[must_use]
fn accelerate(
    wish_dir: Dir3,
    wish_speed: f32,
    velocity: Vec3,
    acceleration_hz: f32,
    ctx: &Ctx,
) -> Vec3 {
    let current_speed = velocity.dot(wish_dir.into());
    let add_speed = wish_speed - current_speed;
    if add_speed <= 0.0 {
        return velocity;
    }

    let accel_speed = f32::min(acceleration_hz * ctx.dt * wish_speed, add_speed);
    velocity + accel_speed * wish_dir
}

fn friction(mut velocity: Vec3, state: &CharacterControllerState, ctx: &Ctx) -> Vec3 {
    let mut vec = velocity;
    if state.walking {
        // ignore slope movement
        vec.y = 0.0;
    }
    let speed = vec.length();
    if speed < 0.05 {
        velocity.x = 0.0;
        velocity.z = 0.0;
        return velocity;
    }
    let drop = if state.walking {
        let stop_speed = f32::max(speed, ctx.cfg.stop_speed);
        stop_speed * ctx.cfg.friction_hz * ctx.dt
    } else {
        0.0
    };

    let new_speed = f32::max(speed - drop, 0.0);
    velocity / speed * new_speed
}

fn check_duck(
    transform: Transform,
    spatial: &SpatialQueryPipeline,
    state: &mut CharacterControllerState,
    ctx: &Ctx,
) {
    if ctx.input.crouched {
        state.crouching = true;
    } else if state.crouching {
        // try to stand up
        state.crouching = false;
        let is_intersecting = is_intersecting(transform, Dir3::Y, spatial, state, ctx);
        state.crouching = is_intersecting;
    }
}

fn ground_trace(
    transform: Transform,
    velocity: Vec3,
    spatial: &SpatialQueryPipeline,
    state: &mut CharacterControllerState,
    ctx: &Ctx,
) {
    let cast_dir = Dir3::NEG_Y;
    let cast_len = ctx.cfg.ground_distance;
    let trace = sweep_check(transform, cast_dir, cast_len, spatial, state, ctx);
    state.previous_grounded = state.grounded;
    state.grounded = trace;

    // if the trace didn't hit anything, we are in free fall
    let Some(mut trace) = trace else {
        ground_trace_missed();
        state.grounded_entity = None;
        state.ground_plane = false;
        state.walking = false;
        return;
    };

    // do something corrective if the trace starts in a solid...
    if trace.distance == 0.0 {
        if let Some(correct_trace) = correct_all_solid(transform, spatial, state, ctx) {
            trace = correct_trace
        } else {
            return;
        }
    }

    // check if getting thrown off the ground
    if velocity.y > 0.0 && velocity.dot(trace.normal1) > ctx.cfg.jump_detection_speed {
        // here we could trigger a jump start event
        state.grounded_entity = None;
        state.ground_plane = false;
        state.walking = false;
        return;
    }

    // slopes that are too steep will not be considered onground
    if trace.normal1.y < ctx.cfg.min_walk_cos {
        state.grounded_entity = None;
        state.ground_plane = true;
        state.walking = false;
        return;
    }

    state.ground_plane = true;
    state.walking = true;
    if state.grounded_entity.is_none() {
        // trigger landing event
        crash_land()
    }
    state.grounded_entity = Some(trace.entity);
}

fn ground_trace_missed() {
    // here we can
    // - trigger transitions into free-fall
    // - do a trace if we are falling quite a bit
    // - if so, trigger the appropriate falling animation
}

fn crash_land() {
    // here we can
    // - check how hard we crashed
    // - trigger crash landing event
    //   - deal damage
    //   - play anims
    //   - reset bob cycle
}

#[must_use]
fn correct_all_solid(
    mut transform: Transform,
    spatial: &SpatialQueryPipeline,
    state: &mut CharacterControllerState,
    ctx: &Ctx,
) -> Option<ShapeHitData> {
    let base = transform;
    for z in -1..1 {
        for x in -1..1 {
            for y in -1..1 {
                let offset = Vec3::new(x as f32, y as f32, z as f32) * 0.05;
                transform.translation = base.translation + offset;
                let mut free = true;
                spatial.shape_intersections_callback(
                    &state.collider(),
                    transform.translation,
                    transform.rotation,
                    &ctx.cfg.filter,
                    |_| {
                        free = false;
                        // stop search
                        false
                    },
                );
                if free {
                    let trace = sweep_check(
                        transform,
                        Dir3::NEG_Y,
                        ctx.cfg.ground_distance,
                        spatial,
                        state,
                        ctx,
                    );
                    state.grounded = trace;
                    return trace;
                }
            }
        }
    }
    state.grounded_entity = None;
    state.walking = false;
    state.ground_plane = false;
    None
}

/// Returns the safe hit distance and the hit data from the spatial query.
/// Source: https://github.com/Ploruto/kcc_prototyping/blob/main/src/move_and_slide.rs#L42
#[must_use]
fn sweep_check(
    transform: Transform,
    direction: Dir3,
    max_distance: f32,
    spatial: &SpatialQueryPipeline,
    state: &CharacterControllerState,
    ctx: &Ctx,
) -> Option<ShapeHitData> {
    let mut hit = spatial.cast_shape(
        &state.collider(),
        transform.translation,
        transform.rotation,
        direction,
        &ShapeCastConfig {
            max_distance: max_distance,
            ignore_origin_penetration: true,
            ..default()
        },
        &ctx.cfg.filter,
    )?;

    let n = hit.normal1;
    let dir: Vec3 = direction.into();
    hit.distance = if n.dot(dir) < 0.0 {
        hit.distance - ctx.cfg.skin_width
    } else {
        let angle_between_hit_normal_and_direction = n.angle_between(-dir);
        let target_distance_to_hit = ctx.cfg.skin_width;
        let hypothenuse = target_distance_to_hit / angle_between_hit_normal_and_direction.cos();
        hit.distance - hypothenuse
    };

    Some(hit)
}

#[must_use]
fn is_intersecting(
    transform: Transform,
    direction: Dir3,
    spatial: &SpatialQueryPipeline,
    state: &CharacterControllerState,
    ctx: &Ctx,
) -> bool {
    let hit = spatial.cast_shape(
        state.collider(),
        transform.translation,
        transform.rotation,
        direction,
        &ShapeCastConfig {
            max_distance: ctx.cfg.skin_width,
            ignore_origin_penetration: true,
            ..default()
        },
        &ctx.cfg.filter,
    );
    hit.is_some()
}
