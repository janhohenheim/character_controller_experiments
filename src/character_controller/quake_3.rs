use std::sync::Arc;

use avian3d::{
    parry::shape::{Capsule, SharedShape},
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
    TranslationInterpolation,
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
    pub(crate) walk_scale: f32,
    pub(crate) acceleration_hz: f32,
    pub(crate) air_acceleration_hz: f32,
    pub(crate) num_bumps: usize,
    pub(crate) gravity: f32,
    pub(crate) step_size: f32,
    pub(crate) max_slope_cosine: f32,
    pub(crate) jump_speed: f32,
    pub(crate) crouch_scale: f32,
    pub(crate) max_speed: f32,
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
            min_walk_cos: 0.766,
            stop_speed: 5.0,
            friction_hz: 6.0,
            walk_scale: 0.5,
            acceleration_hz: 10.0,
            air_acceleration_hz: 1.0,
            num_bumps: 4,
            gravity: 80.0,
            step_size: 1.0,
            max_slope_cosine: 0.7,
            jump_speed: 22.0,
            crouch_scale: 0.25,
            max_speed: 25.0,
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

        if crouching_collider.shape().as_capsule().is_some() {
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
            crouching_collider.set_scale(vec3(1.0, frac, 1.0), 16);
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
                    error!("Error running move_single system: {}", err);
                    continue;
                }
            };
        **world
            .entity_mut(entity)
            .get_mut::<LinearVelocity>()
            .unwrap() = velocity;
        *world.entity_mut(entity).get_mut::<Transform>().unwrap() = transform;
        {
            let mut entity = world.entity_mut(entity);
            let mut current_collider = entity.get_mut::<Collider>().unwrap();
            if !Arc::ptr_eq(&current_collider.shape().0, &state.collider().shape().0) {
                *current_collider = state.collider().clone();
            }
        }
        *world
            .entity_mut(entity)
            .get_mut::<CharacterControllerState>()
            .unwrap() = state;
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
    In((mut transform, mut velocity, mut state, mut ctx)): In<(
        Transform,
        Vec3,
        CharacterControllerState,
        Ctx,
    )>,
    spatial: Res<SpatialQueryPipeline>,
) -> (Transform, Vec3, CharacterControllerState) {
    state.previous_transform = transform;
    state.previous_velocity = velocity;
    scale_inputs(&mut ctx);
    // here we'd handle things like spectator, dead, noclip, etc.

    check_duck(transform, &spatial, &mut state, &ctx);

    ground_trace(transform, velocity, &spatial, &mut state, &ctx);

    (transform, velocity) = if state.walking {
        walk_move(transform, velocity, &spatial, &mut state, &ctx)
    } else {
        air_move(transform, velocity, &spatial, &state, &ctx)
    };
    ground_trace(transform, velocity, &spatial, &mut state, &ctx);

    if let Ok((vel_dir, speed)) = Dir3::new_and_length(velocity) {
        let next_pos = state.previous_transform.translation + vel_dir * speed * ctx.dt;
        let delta = next_pos - state.previous_transform.translation;
        if let Ok((vel_dir, dist)) = Dir3::new_and_length(delta) {
            let new_dist = sweep_check(
                state.previous_transform,
                vel_dir,
                dist,
                &spatial,
                &state,
                &ctx,
            )
            .map(|h| h.distance)
            .unwrap_or(dist);
            velocity = vel_dir * new_dist / ctx.dt;
        }
    }

    (state.previous_transform, velocity, state)
}

fn scale_inputs(ctx: &mut Ctx) {
    let Some(last_movement) = ctx.input.last_movement.as_mut() else {
        return;
    };
    let speed = if ctx.input.walked {
        ctx.cfg.walk_scale
    } else {
        1.0
    };
    *last_movement *= speed;
}

#[must_use]
fn walk_move(
    transform: Transform,
    mut velocity: Vec3,
    spatial: &SpatialQueryPipeline,
    state: &mut CharacterControllerState,
    ctx: &Ctx,
) -> (Transform, Vec3) {
    let jumped: bool;
    (velocity, jumped) = check_jump(velocity, state, ctx);
    if jumped {
        return air_move(transform, velocity, spatial, state, ctx);
    }

    velocity = friction(velocity, state, ctx);
    let scale = cmd_scale(ctx);

    let movement = ctx.input.last_movement.unwrap_or_default();
    let mut forward = Vec3::from(ctx.orientation.forward());
    forward.y = 0.0;
    if let Some(grounded) = state.grounded {
        forward = clip_velocity(forward, grounded.normal1);
    }
    forward = forward.normalize_or_zero();
    let mut right = Vec3::from(ctx.orientation.right());
    right.y = 0.0;
    if let Some(grounded) = state.grounded {
        right = clip_velocity(right, grounded.normal1);
    }
    right = right.normalize_or_zero();

    let wish_vel = movement.y * forward + movement.x * right;
    // when going up or down slopes the wish velocity should not be zero
    // So even if it seems like a good idea, do not do the following:
    // wish_vel.y = 0.0;
    let (wish_dir, mut wish_speed) = Dir3::new_and_length(wish_vel).unwrap_or((Dir3::NEG_Z, 0.0));
    wish_speed *= scale;

    // clamp the speed lower if ducking
    if state.crouching {
        wish_speed = f32::min(wish_speed, ctx.cfg.max_speed * ctx.cfg.crouch_scale);
    }

    velocity = accelerate(wish_dir, wish_speed, velocity, ctx.cfg.acceleration_hz, ctx);

    let acceleration_speed = velocity.length();
    if let Some(grounded) = state.grounded {
        velocity = clip_velocity(velocity, grounded.normal1);
    }

    // don't decrease velocity when going up or down a slope
    velocity = velocity.normalize_or_zero() * acceleration_speed;
    // don't do anything if standing still
    if velocity.xz() == Vec2::ZERO {
        return (transform, velocity);
    }

    step_slide_move(false, transform, velocity, spatial, state, ctx)
}

#[must_use]
fn check_jump(mut velocity: Vec3, state: &mut CharacterControllerState, ctx: &Ctx) -> (Vec3, bool) {
    if !ctx.input.jumped {
        return (velocity, false);
    }
    state.ground_plane = false;
    state.walking = false;
    velocity.y = ctx.cfg.jump_speed;
    // trigger jump event
    (velocity, true)
}

#[must_use]
fn air_move(
    transform: Transform,
    mut velocity: Vec3,
    spatial: &SpatialQueryPipeline,
    state: &CharacterControllerState,
    ctx: &Ctx,
) -> (Transform, Vec3) {
    velocity = friction(velocity, state, ctx);
    let scale = cmd_scale(ctx);

    let movement = ctx.input.last_movement.unwrap_or_default();
    let mut forward = Vec3::from(ctx.orientation.forward());
    forward.y = 0.0;
    forward = forward.normalize_or_zero();
    let mut right = Vec3::from(ctx.orientation.right());
    right.y = 0.0;
    right = right.normalize_or_zero();

    let mut wish_vel = movement.y * forward + movement.x * right;
    wish_vel.y = 0.0;
    let (wish_dir, mut wish_speed) = Dir3::new_and_length(wish_vel).unwrap_or((Dir3::NEG_Z, 0.0));
    wish_speed *= scale;

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
    if state.ground_plane
        && let Some(grounded) = state.grounded
    {
        velocity = clip_velocity(velocity, grounded.normal1);
    }

    step_slide_move(true, transform, velocity, spatial, state, ctx)
}

#[must_use]
fn step_slide_move(
    gravity: bool,
    mut transform: Transform,
    mut velocity: Vec3,
    spatial: &SpatialQueryPipeline,
    state: &CharacterControllerState,
    ctx: &Ctx,
) -> (Transform, Vec3) {
    let start_o = transform;
    let start_v = velocity;

    let clipped: bool;
    (transform, velocity, clipped) = slide_move(gravity, transform, velocity, spatial, state, ctx);
    if !clipped {
        // we got exactly where we wanted to go first try
        return (transform, velocity);
    }
    let cast_dir = Dir3::NEG_Y;
    let cast_dist = ctx.cfg.step_size;
    let trace = sweep_check(start_o, cast_dir, cast_dist, spatial, state, ctx);

    // never step up when you still have up velocity
    if velocity.y > 0.0
        && (trace.is_none()
            || trace.is_some_and(|t| t.normal1.dot(Vec3::Y) < ctx.cfg.max_slope_cosine))
    {
        return (transform, velocity);
    }

    let cast_dir = Dir3::Y;
    // test the player position if they were a stepheight higher
    let trace = sweep_check(start_o, cast_dir, cast_dist, spatial, state, ctx);
    let step_size = if let Some(trace) = trace {
        trace.distance
    } else {
        cast_dist
    };
    if is_intersecting(start_o, spatial, state, ctx) {
        // can't step up
        return (transform, velocity);
    }

    // try slidemove from this position
    transform.translation = start_o.translation + cast_dir * step_size;
    velocity = start_v;

    (transform, velocity, _) = slide_move(gravity, transform, velocity, spatial, state, ctx);

    // push down the final amount
    let cast_dir = Dir3::NEG_Y;
    let cast_dist = step_size;
    let trace = sweep_check(transform, cast_dir, cast_dist, spatial, state, ctx);
    if let Some(trace) = trace {
        transform.translation += cast_dir * trace.distance;
        velocity = clip_velocity(velocity, trace.normal1);
    } else {
        transform.translation += cast_dir * cast_dist;
    }
    (transform, velocity)
}

#[must_use]
fn slide_move(
    gravity: bool,
    mut transform: Transform,
    mut velocity: Vec3,
    spatial: &SpatialQueryPipeline,
    state: &CharacterControllerState,
    ctx: &Ctx,
) -> (Transform, Vec3, bool) {
    let mut end_velocity = Vec3::ZERO;

    if gravity {
        end_velocity = velocity;
        end_velocity.y -= ctx.dt * ctx.cfg.gravity;
        velocity.y = (velocity.y + end_velocity.y) * 0.5;
        if state.ground_plane
            && let Some(grounded) = state.grounded
        {
            // slide along the ground plane
            velocity = clip_velocity(velocity, grounded.normal1);
        }
    }

    let mut time_left = ctx.dt;

    const MAX_CLIP_PLANES: usize = 5;
    let mut planes = [Vec3::ZERO; MAX_CLIP_PLANES];
    // never turn against the ground plane
    let mut num_planes = if state.ground_plane
        && let Some(grounded) = state.grounded
    {
        planes[0] = grounded.normal1;
        1
    } else {
        0
    };

    // never turn against original velocity
    planes[num_planes] = velocity.normalize_or_zero();
    num_planes += 1;

    let mut bump_count = 0;
    while bump_count < ctx.cfg.num_bumps {
        // calculate position we are trying to move to
        let (cast_dir, cast_len) =
            Dir3::new_and_length(time_left * velocity).unwrap_or((Dir3::Z, 0.0));

        // see if we can make it there
        let trace = sweep_check(transform, cast_dir, cast_len, spatial, state, ctx);

        let Some(trace) = trace else {
            // moved the entire distance
            transform.translation += cast_dir * cast_len;
            break;
        };
        if trace.distance == 0.0 {
            // entity is completely trapped in another solid
            // don't build up falling damage, but allow sideways acceleration
            velocity.y = 0.0;
            return (transform, velocity, true);
        }
        transform.translation += cast_dir * trace.distance;

        // trigger touch event here
        time_left -= time_left * trace.distance / cast_len;

        if num_planes >= MAX_CLIP_PLANES {
            return (transform, Vec3::ZERO, true);
        }
        // if this is the same plane we hit before, nudge velocity
        // out along it, which fixes some epsilon issues with
        // non-axial planes
        let mut i = 0;
        while i < num_planes {
            if trace.normal1.dot(planes[i]) > 0.99 {
                velocity += trace.normal1 * 0.05;
                break;
            }
            i += 1;
        }
        if i < num_planes {
            bump_count += 1;
            continue;
        }
        planes[num_planes] = trace.normal1;
        num_planes += 1;

        // modify velocity so it parallels all of the clip planes

        // find a plane that it enters
        for i in 0..num_planes {
            let into = velocity.dot(planes[i]);
            if into >= 0.005 {
                // move doesn't interact with the plane
                continue;
            }

            // see how hard we are hitting things
            // could use `-into` as the impact speed here and accumulate it
            // i.e. slide_impact_speed = min(-into, slide_impact_speed)
            // but Q3 doesn't actually read the value later

            // slide along the plane
            let mut current_clip_velocity = clip_velocity(velocity, planes[i]);
            let mut end_clip_velocity = clip_velocity(end_velocity, planes[i]);

            // see if there is a second plane that the new move enters
            for j in 0..num_planes {
                if j == i {
                    continue;
                }
                if current_clip_velocity.dot(planes[j]) >= 0.005 {
                    // move doesn't interact with the plane
                    continue;
                }

                // try clipping the move to the plane
                current_clip_velocity = clip_velocity(current_clip_velocity, planes[j]);
                end_clip_velocity = clip_velocity(end_clip_velocity, planes[j]);

                // see if it goes back into the first clip plane
                if current_clip_velocity.dot(planes[i]) >= 0.0 {
                    continue;
                }

                // slide the original velocity along the crease
                let dir = planes[i].cross(planes[j]).normalize();
                let d = dir.dot(velocity);
                current_clip_velocity = dir * d;

                let d = dir.dot(end_velocity);
                end_clip_velocity = dir * d;

                // see if there is a third plane the the new move enters
                for k in 0..num_planes {
                    if k == i || k == j {
                        continue;
                    }

                    if current_clip_velocity.dot(planes[k]) >= 0.005 {
                        // move doesn't interact with the plane
                        continue;
                    }

                    // stop dead at a tripple plane interaction
                    return (transform, Vec3::ZERO, true);
                }
            }
            // if we have fixed all interactions, try another move
            velocity = current_clip_velocity;
            end_velocity = end_clip_velocity;
            break;
        }

        bump_count += 1;
    }
    if gravity {
        velocity = end_velocity;
    }

    // now Q3 confusedly resets the velocity if in a timer

    (transform, velocity, bump_count != 0)
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
        let is_intersecting =
            sweep_check(transform, Dir3::Y, ctx.cfg.skin_width, spatial, state, ctx).is_some();
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
            trace = correct_trace;
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
    for z in -1..=1 {
        for x in -1..=1 {
            for y in -1..=1 {
                let offset = Vec3::new(x as f32, y as f32, z as f32) * 0.05;
                transform.translation = base.translation + offset;
                if !is_intersecting(transform, spatial, state, ctx) {
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
        state.collider(),
        transform.translation,
        transform.rotation,
        direction,
        &ShapeCastConfig {
            max_distance,
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
    hit.distance = hit.distance.max(0.0);

    Some(hit)
}

#[must_use]
fn is_intersecting(
    transform: Transform,
    spatial: &SpatialQueryPipeline,
    state: &CharacterControllerState,
    ctx: &Ctx,
) -> bool {
    let mut intersects = false;
    spatial.shape_intersections_callback(
        state.collider(),
        transform.translation,
        transform.rotation,
        &ctx.cfg.filter,
        |_| {
            intersects = true;
            false
        },
    );
    intersects
}

#[must_use]
fn cmd_scale(ctx: &Ctx) -> f32 {
    let Some(mov) = ctx.input.last_movement else {
        return 0.0;
    };
    let max = f32::max(mov.x.abs(), mov.y.abs());
    if max == 0.0 {
        return 0.0;
    }
    let total = mov.length();
    ctx.cfg.max_speed * max / total
}
