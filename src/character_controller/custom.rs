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
    CharacterControllerCamera, CharacterControllerSystems, input::AccumulatedInput,
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
    pub(crate) standing_view_height: f32,
    pub(crate) crouch_view_height: f32,
    pub(crate) ground_distance: f32,
    pub(crate) jump_detection_speed: f32,
    pub(crate) min_walk_cos: f32,
    pub(crate) stop_speed: f32,
    pub(crate) friction_hz: f32,
    pub(crate) acceleration_hz: f32,
    pub(crate) air_acceleration_hz: f32,
    pub(crate) gravity: f32,
    pub(crate) step_size: f32,
    pub(crate) jump_speed: f32,
    pub(crate) crouch_scale: f32,
    pub(crate) speed: f32,
    pub(crate) air_speed: f32,
    pub(crate) move_and_slide: MoveAndSlideConfig,
}

impl Default for CharacterController {
    fn default() -> Self {
        Self {
            crouch_height: 1.3,
            filter: SpatialQueryFilter::default(),
            standing_view_height: 1.7,
            crouch_view_height: 1.2,
            ground_distance: 0.015,
            jump_detection_speed: 0.5,
            min_walk_cos: 0.766,
            stop_speed: 5.0,
            friction_hz: 8.0,
            acceleration_hz: 12.0,
            air_acceleration_hz: 1.5,
            gravity: 50.0,
            step_size: 1.0,
            jump_speed: 14.3,
            crouch_scale: 0.25,
            speed: 18.0,
            air_speed: 2.0,
            move_and_slide: MoveAndSlideConfig {
                skin_width: 0.003,
                ..default()
            },
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
    pub(crate) velocity: Vec3,
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
    );
}

fn run_kcc(
    world: &mut World,
    mut kccs: Local<
        QueryState<(
            Entity,
            &CharacterController,
            &CharacterControllerState,
            &AccumulatedInput,
            &Transform,
            Option<&CharacterControllerCamera>,
        )>,
    >,
    mut scratch: Local<Vec<(Transform, CharacterControllerState, Ctx)>>,
) {
    scratch.extend(
        kccs.iter(world)
            .map(|(entity, cfg, state, input, transform, camera)| {
                (
                    *transform,
                    state.clone(),
                    Ctx {
                        entity,
                        cfg: cfg.clone(),
                        input: *input,
                        orientation: camera
                            .and_then(|e| world.entity(e.0).get::<Transform>().copied())
                            .unwrap_or(*transform),
                    },
                )
            }),
    );
    for (transform, state, ctx) in scratch.drain(..) {
        let entity = ctx.entity;
        let (position, state): (Vec3, CharacterControllerState) =
            match world.run_system_cached_with(move_single, (transform, state, ctx)) {
                Ok(val) => val,
                Err(err) => {
                    error!("Error running move_single system: {}", err);
                    continue;
                }
            };
        world
            .entity_mut(entity)
            .get_mut::<Transform>()
            .unwrap()
            .translation = position;
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
}

#[must_use]
fn move_single(
    In((mut transform, mut state, ctx)): In<(Transform, CharacterControllerState, Ctx)>,
    move_and_slide: MoveAndSlide,
) -> (Vec3, CharacterControllerState) {
    let original_transform = transform;
    let mut velocity = state.velocity;
    // here we'd handle things like spectator, dead, noclip, etc.

    check_duck(transform, &move_and_slide, &mut state, &ctx);

    ground_trace(transform, velocity, &move_and_slide, &mut state, &ctx);

    (transform, velocity) = if state.walking {
        walk_move(transform, velocity, &move_and_slide, &mut state, &ctx)
    } else {
        air_move(transform, velocity, &move_and_slide, &state, &ctx)
    };
    ground_trace(transform, velocity, &move_and_slide, &mut state, &ctx);

    transform = dejitter_output(original_transform, transform);

    state.velocity = velocity;

    (transform.translation, state)
}

fn dejitter_output(original_transform: Transform, mut transform: Transform) -> Transform {
    const EPSILON: f32 = 0.0005;

    for i in 0..3 {
        let delta_pos = original_transform.translation - transform.translation;
        if delta_pos[i].abs() < EPSILON {
            transform.translation[i] = original_transform.translation[i];
        }
    }

    transform
}

#[must_use]
fn walk_move(
    transform: Transform,
    mut velocity: Vec3,
    move_and_slide: &MoveAndSlide,
    state: &mut CharacterControllerState,
    ctx: &Ctx,
) -> (Transform, Vec3) {
    let jumped: bool;
    (velocity, jumped) = check_jump(velocity, state, ctx);
    if jumped {
        return air_move(transform, velocity, move_and_slide, state, ctx);
    }

    velocity = friction(velocity, state, move_and_slide, ctx);
    let scale = cmd_scale(ctx);

    let movement = ctx.input.last_movement.unwrap_or_default();
    let mut forward = Vec3::from(ctx.orientation.forward());
    forward.y = 0.0;
    if let Some(grounded) = state.grounded {
        forward = MoveAndSlide::clip_velocity(forward, &[grounded.normal1.try_into().unwrap()]);
    }
    forward = forward.normalize_or_zero();
    let mut right = Vec3::from(ctx.orientation.right());
    right.y = 0.0;
    if let Some(grounded) = state.grounded {
        right = MoveAndSlide::clip_velocity(right, &[grounded.normal1.try_into().unwrap()]);
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
        wish_speed = f32::min(wish_speed, ctx.cfg.speed * ctx.cfg.crouch_scale);
    }

    velocity = accelerate(wish_dir, wish_speed, velocity, move_and_slide, ctx);

    let acceleration_speed = velocity.length();

    if let Some(grounded) = state.grounded {
        velocity = MoveAndSlide::clip_velocity(velocity, &[grounded.normal1.try_into().unwrap()]);
    }

    // don't decrease velocity when going up or down a slope
    velocity = velocity.normalize_or_zero() * acceleration_speed;
    // don't do anything if standing still
    if velocity.xz() == Vec2::ZERO {
        return (transform, velocity);
    }

    step_slide_move(false, transform, velocity, move_and_slide, state, ctx)
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
    move_and_slide: &MoveAndSlide,
    state: &CharacterControllerState,
    ctx: &Ctx,
) -> (Transform, Vec3) {
    velocity = friction(velocity, state, move_and_slide, ctx);
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
    velocity = air_accelerate(wish_dir, wish_speed, velocity, move_and_slide, ctx);

    // we may have a ground plane that is very steep, even
    // though we don't have a groundentity
    // slide along the steep plane
    if state.ground_plane
        && let Some(grounded) = state.grounded
    {
        velocity = MoveAndSlide::clip_velocity(velocity, &[grounded.normal1.try_into().unwrap()]);
    }

    step_slide_move(true, transform, velocity, move_and_slide, state, ctx)
}

#[must_use]
fn step_slide_move(
    gravity: bool,
    mut transform: Transform,
    mut velocity: Vec3,
    move_and_slide: &MoveAndSlide,
    state: &CharacterControllerState,
    ctx: &Ctx,
) -> (Transform, Vec3) {
    if gravity {
        velocity += Vec3::NEG_Y * ctx.cfg.gravity * move_and_slide.time.delta_secs();
    }

    let start_o = transform;
    let start_v = velocity;

    let mut clipped = false;
    let result = move_and_slide.move_and_slide(
        state.collider(),
        transform.translation,
        transform.rotation,
        velocity,
        &ctx.cfg.move_and_slide,
        &ctx.cfg.filter,
        |_| {
            clipped = true;
            true
        },
    );
    transform.translation = result.position;
    velocity = result.clipped_velocity;

    // Non-Quake: also don't step in the air
    if !clipped || !state.walking {
        // we got exactly where we wanted to go first try
        return (transform, velocity);
    }
    let direct_transform = transform;
    let direct_velocity = velocity;

    let cast_dir = Dir3::NEG_Y;
    let cast_dist = ctx.cfg.step_size;
    let trace = move_and_slide.query_pipeline.cast_shape(
        state.collider(),
        start_o.translation,
        start_o.rotation,
        cast_dir,
        &ShapeCastConfig::from_max_distance(cast_dist),
        &ctx.cfg.filter,
    );

    // never step up when you still have up velocity
    if velocity.y > 0.0
        && (trace.is_none() || trace.is_some_and(|t| t.normal1.dot(Vec3::Y) < ctx.cfg.min_walk_cos))
    {
        return (transform, velocity);
    }

    let cast_dir = Dir3::Y;
    // test the player position if they were a stepheight higher
    let sweep_hit = move_and_slide.cast_move(
        state.collider(),
        start_o.translation,
        start_o.rotation,
        cast_dir * cast_dist,
        ctx.cfg.move_and_slide.skin_width,
        &ctx.cfg.filter,
    );
    let step_size = if let Some(sweep_hit) = sweep_hit {
        sweep_hit.distance
    } else {
        cast_dist
    };
    if step_size <= 0.0 {
        // can't step up
        return (transform, velocity);
    }

    // try slidemove from this position
    transform.translation = start_o.translation + cast_dir * step_size;
    transform.translation += move_and_slide.depenetrate(
        state.collider(),
        transform.translation,
        transform.rotation,
        &(&ctx.cfg.move_and_slide).into(),
        &ctx.cfg.filter,
    );
    velocity = start_v;

    let result = move_and_slide.move_and_slide(
        state.collider(),
        transform.translation,
        transform.rotation,
        velocity,
        &ctx.cfg.move_and_slide,
        &ctx.cfg.filter,
        |_| true,
    );
    transform.translation = result.position;
    velocity = result.clipped_velocity;

    // push down the final amount
    let cast_dir = Dir3::NEG_Y;
    let cast_dist = step_size;
    let sweep_hit = move_and_slide.cast_move(
        state.collider(),
        transform.translation,
        transform.rotation,
        cast_dir * cast_dist,
        ctx.cfg.move_and_slide.skin_width,
        &ctx.cfg.filter,
    );
    if let Some(sweep_hit) = sweep_hit {
        transform.translation += cast_dir * sweep_hit.distance;
        velocity = MoveAndSlide::clip_velocity(velocity, &[sweep_hit.normal1.try_into().unwrap()]);
    } else {
        transform.translation += cast_dir * cast_dist;
    }
    transform.translation += move_and_slide.depenetrate(
        state.collider(),
        transform.translation,
        transform.rotation,
        &(&ctx.cfg.move_and_slide).into(),
        &ctx.cfg.filter,
    );

    // non-Quake code incoming: if we
    // - didn't really step up
    // - stepped onto something we would slide down from
    // let's not step at all. That eliminates nasty situations where we get "ghost steps" when penetrating walls.
    let direct_horizontal_dist = start_o
        .translation
        .xz()
        .distance_squared(direct_transform.translation.xz());
    let step_horizontal_dist = start_o
        .translation
        .xz()
        .distance_squared(transform.translation.xz());
    let did_not_advance_through_stepping = direct_horizontal_dist >= step_horizontal_dist - 0.001;

    if did_not_advance_through_stepping || trace.is_some_and(|t| t.normal1.y < ctx.cfg.min_walk_cos)
    {
        (direct_transform, direct_velocity)
    } else {
        (transform, velocity)
    }
}

#[must_use]
fn accelerate(
    wish_dir: Dir3,
    wish_speed: f32,
    velocity: Vec3,
    move_and_slide: &MoveAndSlide,
    ctx: &Ctx,
) -> Vec3 {
    let current_speed = velocity.dot(wish_dir.into());
    let add_speed = wish_speed - current_speed;
    if add_speed <= 0.0 {
        return velocity;
    }

    let accel_speed = f32::min(
        ctx.cfg.acceleration_hz * move_and_slide.time.delta_secs() * wish_speed,
        add_speed,
    );
    velocity + accel_speed * wish_dir
}

#[must_use]
fn air_accelerate(
    wish_dir: Dir3,
    wish_speed: f32,
    velocity: Vec3,
    move_and_slide: &MoveAndSlide,
    ctx: &Ctx,
) -> Vec3 {
    let current_speed = velocity.dot(wish_dir.into());
    // right here is where air strafing happens: `current_speed` is close to 0 when we want to move perpendicular to
    // our current velocity, making `add_speed` large.
    let air_wish_speed = f32::min(wish_speed, ctx.cfg.air_speed);
    let add_speed = air_wish_speed - current_speed;
    if add_speed <= 0.0 {
        return velocity;
    }

    let accel_speed = f32::min(
        ctx.cfg.acceleration_hz * move_and_slide.time.delta_secs() * wish_speed,
        add_speed,
    );
    velocity + accel_speed * wish_dir
}

fn friction(
    mut velocity: Vec3,
    state: &CharacterControllerState,
    move_and_slide: &MoveAndSlide,
    ctx: &Ctx,
) -> Vec3 {
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
        stop_speed * ctx.cfg.friction_hz * move_and_slide.time.delta_secs()
    } else {
        0.0
    };

    let new_speed = f32::max(speed - drop, 0.0);
    velocity / speed * new_speed
}

fn check_duck(
    transform: Transform,
    move_and_slide: &MoveAndSlide,
    state: &mut CharacterControllerState,
    ctx: &Ctx,
) {
    if ctx.input.crouched {
        state.crouching = true;
    } else if state.crouching {
        // try to stand up
        state.crouching = false;
        let is_intersecting = is_intersecting(transform, state, move_and_slide, ctx);
        state.crouching = is_intersecting;
    }
}

fn ground_trace(
    transform: Transform,
    velocity: Vec3,
    move_and_slide: &MoveAndSlide,
    state: &mut CharacterControllerState,
    ctx: &Ctx,
) {
    let cast_dir = Dir3::NEG_Y;
    let cast_dist = ctx.cfg.ground_distance;
    let trace = move_and_slide.query_pipeline.cast_shape(
        state.collider(),
        transform.translation,
        transform.rotation,
        cast_dir,
        &ShapeCastConfig::from_max_distance(cast_dist),
        &ctx.cfg.filter,
    );
    state.previous_grounded = state.grounded;
    state.grounded = trace;

    // if the trace didn't hit anything, we are in free fall
    let Some(trace) = trace else {
        ground_trace_missed();
        state.grounded_entity = None;
        state.ground_plane = false;
        state.walking = false;
        return;
    };

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
fn cmd_scale(ctx: &Ctx) -> f32 {
    let Some(mov) = ctx.input.last_movement else {
        return 0.0;
    };
    let max = f32::max(mov.x.abs(), mov.y.abs());
    if max == 0.0 {
        return 0.0;
    }
    let total = mov.length();
    ctx.cfg.speed * max / total
}

#[must_use]
fn is_intersecting(
    transform: Transform,
    state: &CharacterControllerState,
    move_and_slide: &MoveAndSlide,
    ctx: &Ctx,
) -> bool {
    !move_and_slide
        .intersections(
            state.collider(),
            transform.translation,
            transform.rotation,
            // No need to worry about skin width, depenetration will take care of it
            0.0,
            &ctx.cfg.filter,
        )
        .is_empty()
}
