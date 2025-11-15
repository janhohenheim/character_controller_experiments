use avian3d::prelude::*;
use bevy::{
    ecs::{
        lifecycle::HookContext, relationship::RelationshipSourceCollection as _,
        system::SystemState, world::DeferredWorld,
    },
    prelude::*,
};

use crate::character_controller::{
    CharacterControllerForward, CharacterControllerSystems, input::AccumulatedInput,
};

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
    /// The speed at which the character can move on the ground.
    /// X is horizontal speed, Y is vertical speed.
    ///
    /// Default: [10 m/s, 9 m/s]
    pub(crate) speed: Vec2,
    /// The speed at which the character can move in the air.
    ///
    /// Default: 0.7 m/s
    pub(crate) air_speed: f32,
    /// Idk acceleration constant
    ///
    /// Default: 10 Hz
    pub(crate) acceleration_hz: f32,
    /// The maximum total speed the character can reach on the ground while moving into the forward direction.
    /// Note that this speed can be easily outrun by all kinds of strafing.
    ///
    /// Default: 8 m/s
    pub(crate) max_speed: f32,
    /// The gravity acceleration applied to the character.
    ///
    /// Default: 20 m/s²
    pub(crate) gravity: f32,
    /// Idk a constant for friction lol
    ///
    /// Default: 4 Hz
    pub(crate) friction_hz: f32,
    /// The minimum speed with which the character aborts movement when no longer trying to move.
    /// The actual stopping speed can be higher than this value due to friction.
    ///
    /// Default: 2.5 m/s
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
    /// The filter for collisions.
    /// Implicitly always excludes the character's own entity.
    pub(crate) filter: SpatialQueryFilter,
    /// The distance kept between the character and colliders. Needed for the character to not penetrate colliders.
    ///
    /// Default: 0.01 m
    pub(crate) skin_width: f32,
    /// The maximum height the character can step up.
    ///
    /// Default: 0.5 m
    pub(crate) step_height: f32,
    /// The speed at which the character jumps.
    ///
    /// Default: 7 m/s
    pub(crate) jump_speed: f32,
}

impl Default for CharacterController {
    fn default() -> Self {
        Self {
            speed: vec2(9., 10.),
            air_speed: 0.7,
            acceleration_hz: 10.0,
            max_speed: 8.0,
            gravity: 20.,
            friction_hz: 4.0,
            stop_speed: 2.5,
            // 45.57 degrees
            max_slope_cosine: 0.7,
            airborne_speed: 4.5,
            skin_width: 0.01,
            step_height: 0.5,
            filter: SpatialQueryFilter::default(),
            jump_speed: 7.0,
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
            &AccumulatedInput,
            &LinearVelocity,
            &GlobalTransform,
            &Collider,
            &ColliderAabb,
            Option<&CharacterControllerForward>,
        )>,
    >,
    mut transforms: Local<QueryState<&GlobalTransform>>,
    mut scratch: Local<Vec<(Transform, Vec3, Ctx)>>,
    mut transform_helper: Local<SystemState<TransformHelper>>,
) {
    let dt = world.resource::<Time>().delta_secs();
    scratch.extend(kccs.iter(world).map(
        |(entity, cfg, input, vel, transform, collider, aabb, forward)| {
            let transform = transform.compute_transform();
            (
                transform,
                vel.0,
                Ctx {
                    entity,
                    cfg: cfg.clone(),
                    input: *input,
                    dt,
                    aabb: *aabb,
                    orientation: forward
                        .and_then(|e| {
                            transforms
                                .get(world, e.0)
                                .map(|t| t.compute_transform())
                                .ok()
                        })
                        .unwrap_or(transform),
                    collider: collider.clone(),
                },
            )
        },
    ));
    for (transform, velocity, ctx) in scratch.drain(..) {
        let entity = ctx.entity;
        let (transform, velocity, grounded): (Transform, Vec3, Option<Entity>) =
            match world.run_system_cached_with(player_move, (transform, velocity, ctx)) {
                Ok(val) => val,
                Err(err) => {
                    error!("Error running air_move system: {}", err);
                    continue;
                }
            };
        let global_transform = match transform_helper.get(world).compute_global_transform(entity) {
            Ok(global_transform) => global_transform,
            Err(err) => {
                error!("Error computing global transform: {err}. Skipping entity.");
                transform.into()
            }
        };
        **world
            .entity_mut(entity)
            .get_mut::<LinearVelocity>()
            .unwrap() = velocity;
        world
            .entity_mut(entity)
            .get_mut::<CharacterControllerState>()
            .unwrap()
            .grounded = grounded;
        *world.entity_mut(entity).get_mut::<Transform>().unwrap() = transform;
        *world
            .entity_mut(entity)
            .get_mut::<GlobalTransform>()
            .unwrap() = global_transform;
    }
}

#[derive(Debug)]
struct Ctx {
    entity: Entity,
    orientation: Transform,
    cfg: CharacterController,
    input: AccumulatedInput,
    aabb: ColliderAabb,
    collider: Collider,
    dt: f32,
}

#[must_use]
fn player_move(
    In((mut transform, mut velocity, ctx)): In<(Transform, Vec3, Ctx)>,
    spatial: Res<SpatialQueryPipeline>,
) -> (Transform, Vec3, Option<Entity>) {
    transform = nudge_position(transform, &spatial, &ctx);

    let mut grounded: Option<Entity>;
    (transform, grounded) = categorize_position(transform, velocity, &ctx, &spatial);

    (grounded, velocity) = jump_button(velocity, grounded, &ctx);

    velocity = friction(transform, velocity, grounded, &ctx, &spatial);
    (transform, velocity) = air_move(transform, velocity, grounded, &spatial, &ctx);

    (transform, grounded) = categorize_position(transform, velocity, &ctx, &spatial);

    (transform, velocity, grounded)
}

#[must_use]
fn air_move(
    mut transform: Transform,
    mut velocity: Vec3,
    grounded: Option<Entity>,
    spatial: &SpatialQueryPipeline,
    ctx: &Ctx,
) -> (Transform, Vec3) {
    let movement = ctx.input.last_movement.unwrap_or_default();
    let cfg_speed = ctx.cfg.speed;
    let mut wish_vel = cfg_speed.y * movement.y * ctx.orientation.forward()
        + cfg_speed.x * movement.x * ctx.orientation.right();
    wish_vel.y = 0.0;
    let (wish_dir, mut wish_speed) = Dir3::new_and_length(wish_vel).unwrap_or((Dir3::NEG_Z, 0.0));
    if wish_speed > ctx.cfg.max_speed {
        wish_vel *= ctx.cfg.max_speed / wish_speed;
        wish_speed = ctx.cfg.max_speed;
    }

    if grounded.is_some() {
        velocity.y = 0.0;
        velocity = accelerate(wish_dir, wish_speed, velocity, ctx);
        velocity.y -= ctx.cfg.gravity * ctx.dt;
        (transform, velocity) = ground_move(transform, velocity, ctx, spatial);
    } else {
        velocity = air_accelerate(wish_dir, wish_speed, velocity, ctx);
        velocity.y -= ctx.cfg.gravity * ctx.dt;
        (transform, velocity) = fly_move(transform, velocity, ctx, spatial);
    }
    (transform, velocity)
}

#[must_use]
fn ground_move(
    mut transform: Transform,
    mut velocity: Vec3,
    ctx: &Ctx,
    spatial: &SpatialQueryPipeline,
) -> (Transform, Vec3) {
    velocity.y = 0.0;
    if velocity == Vec3::ZERO {
        return (transform, velocity);
    }
    // first try just moving to the destination
    let mut cast_dir = velocity * ctx.dt;
    cast_dir.y = 0.0;

    let Ok((cast_dir, cast_len)) = Dir3::new_and_length(cast_dir) else {
        return (transform, velocity);
    };

    // first try moving directly to the next spot
    let trace = sweep_check(transform, cast_dir, cast_len, spatial, ctx);
    if trace.is_none() {
        transform.translation += cast_dir * cast_len;
        return (transform, velocity);
    };

    // try sliding forward both on ground and up 16 inches
    // take the move that goes farthest

    let original = transform.translation;

    // this is how much we move if we stay on the ground
    let (down, down_velocity) = fly_move(transform, velocity, ctx, spatial);

    // move up a stair height
    let cast_len = ctx.cfg.step_height;
    let cast_dir = Dir3::Y;
    let trace = sweep_check(transform, cast_dir, cast_len, spatial, ctx);
    if let Some(trace) = trace {
        transform.translation += cast_dir * trace.distance;
    } else {
        transform.translation += cast_dir * cast_len;
    }

    // move after going up some height
    (transform, velocity) = fly_move(transform, velocity, ctx, spatial);

    // press down the step height
    let cast_dir = Dir3::NEG_Y;
    let trace = sweep_check(transform, cast_dir, cast_len, spatial, ctx);
    if let Some(trace) = trace {
        // Treat slopes above a certain angle as falling. This is where surf mechanics come from!!
        if trace.normal1.y < ctx.cfg.max_slope_cosine {
            return (down, down_velocity);
        }
        transform.translation += cast_dir * trace.distance;
    } else {
        transform.translation += cast_dir * cast_len;
    }

    let up = transform.translation;
    // decide which one went farther
    // down: what if we moved on the ground?
    // up: what if we
    // - teleported a bit up to not bump into stairs
    // - moved
    // - then teleported back down onto the ground?
    let down_dist = down.translation.xz().distance_squared(original.xz());
    let up_dist = up.xz().distance_squared(original.xz());

    if down_dist > up_dist {
        (down, down_velocity)
    } else {
        // copy z value from slide move
        velocity.y = down_velocity.y;
        (transform, velocity)
    }
}

#[must_use]
fn fly_move(
    mut transform: Transform,
    mut velocity: Vec3,
    ctx: &Ctx,
    spatial: &SpatialQueryPipeline,
) -> (Transform, Vec3) {
    let mut time_left = ctx.dt;
    const NUM_BUMPS: usize = 4;
    const MAX_CLIP_PLANES: usize = 5;
    let original_velocity = velocity;
    let mut num_planes = 0;
    let mut planes = [Vec3::ZERO; MAX_CLIP_PLANES];

    for _ in 0..NUM_BUMPS {
        let Ok((cast_dir, cast_len)) = Dir3::new_and_length(time_left * velocity) else {
            break;
        };
        let trace = sweep_check(transform, cast_dir, cast_len, spatial, ctx);
        let Some(trace) = trace else {
            transform.translation += cast_dir * cast_len;
            // moved the entire distance
            break;
        };
        transform.translation += cast_dir * trace.distance;

        if trace.distance > 0.0 {
            // actually covered some distance
            num_planes = 0;
        }

        let fraction = trace.distance / cast_len;
        // account for the time the cast spent while tracing
        time_left -= time_left * fraction;

        if num_planes >= MAX_CLIP_PLANES {
            velocity = Vec3::ZERO;
            break;
        }
        planes[num_planes] = trace.normal1;
        num_planes += 1;
        let mut i = 0;
        while i < num_planes {
            velocity = clip_velocity(original_velocity, planes[i], 1.0);
            let mut j = 0;
            while j < num_planes {
                if i != j && velocity.dot(planes[j]) < 0.0 {
                    break;
                }
                j += 1;
            }
            if j == num_planes {
                break;
            }
            i += 1;
        }
        if i != num_planes {
            // go along this plane
        } else {
            // go along the crease
            if num_planes != 2 {
                velocity = Vec3::ZERO;
                break;
            }
            let dir = planes[0].cross(planes[1]);
            let d = dir.dot(velocity);
            velocity = dir * d;
        }
        // if original velocity is against the original velocity, stop dead
        // to avoid tiny occilations in sloping corners
        if velocity.dot(original_velocity) <= 0.0 {
            velocity = Vec3::ZERO;
            break;
        }
    }
    (transform, velocity)
}

#[must_use]
fn clip_velocity(velocity: Vec3, normal: Vec3, overbounce: f32) -> Vec3 {
    let backoff = velocity.dot(normal) * overbounce;
    let change = normal * backoff;
    let mut new_velocity = velocity - change;
    const STOP_EPSILON: f32 = 0.0025;
    for i in 0..3 {
        if new_velocity[i].abs() < STOP_EPSILON {
            new_velocity[i] = 0.0;
        }
    }
    new_velocity
}

#[must_use]
fn accelerate(wish_dir: Dir3, wish_speed: f32, velocity: Vec3, ctx: &Ctx) -> Vec3 {
    let current_speed = velocity.dot(wish_dir.into());
    let add_speed = wish_speed - current_speed;
    if add_speed <= 0.0 {
        return velocity;
    }

    let accel_speed = f32::min(ctx.cfg.acceleration_hz * ctx.dt * wish_speed, add_speed);
    velocity + accel_speed * wish_dir
}

#[must_use]
fn air_accelerate(wish_dir: Dir3, wish_speed: f32, velocity: Vec3, ctx: &Ctx) -> Vec3 {
    let current_speed = velocity.dot(wish_dir.into());
    // right here is where air strafing happens: `current_speed` is close to 0 when we want to move perpendicular to
    // our current velocity, making `add_speed` large.
    let air_wish_speed = f32::min(wish_speed, ctx.cfg.air_speed);
    let add_speed = air_wish_speed - current_speed;
    if add_speed <= 0.0 {
        return velocity;
    }

    let accel_speed = f32::min(ctx.cfg.acceleration_hz * ctx.dt * wish_speed, add_speed);
    velocity + accel_speed * wish_dir
}

#[must_use]
fn friction(
    transform: Transform,
    mut velocity: Vec3,
    grounded: Option<Entity>,
    ctx: &Ctx,
    spatial: &SpatialQueryPipeline,
) -> Vec3 {
    let speed = velocity.length();
    if speed < 0.025 {
        velocity.x = 0.0;
        velocity.z = 0.0;
        return velocity;
    }

    let mut friction_hz = ctx.cfg.friction_hz;
    // if the leading edge is over a dropoff, increase friction
    if grounded.is_some() {
        // speed cannot be zero, we early return in that case already
        let vel_dir = velocity / speed;
        let mut start = transform;
        start.translation += vel_dir * 0.4;
        // min is negative, so this goes *down*
        start.translation.y = transform.translation.y + ctx.aabb.min.y;
        let cast_dir = Dir3::NEG_Y;

        let trace = sweep_check(start, cast_dir, 0.85, spatial, ctx);
        if trace.is_none() {
            friction_hz *= 2.0;
        }
    }
    let drop = if grounded.is_some() {
        let stop_speed = f32::max(speed, ctx.cfg.stop_speed);
        stop_speed * friction_hz * ctx.dt
    } else {
        0.0
    };
    let new_speed = f32::max(speed - drop, 0.0);
    velocity / speed * new_speed
}

#[must_use]
fn categorize_position(
    mut transform: Transform,
    velocity: Vec3,
    ctx: &Ctx,
    spatial: &SpatialQueryPipeline,
) -> (Transform, Option<Entity>) {
    if velocity.y > ctx.cfg.airborne_speed {
        return (transform, None);
    }
    let cast_len = 0.025;
    let cast_dir = Dir3::NEG_Y;
    let trace = sweep_check(transform, cast_dir, cast_len, spatial, ctx);
    let Some(trace) = trace else {
        return (transform, None);
    };
    // Treat slopes above a certain angle as falling. This is where surf mechanics come from!!
    if trace.normal1.y < ctx.cfg.max_slope_cosine {
        return (transform, None);
    };
    transform.translation += cast_dir * trace.distance;
    let grounded = trace.entity;

    (transform, Some(grounded))
}

#[must_use]
fn nudge_position(
    mut transform: Transform,
    spatial: &SpatialQueryPipeline,
    ctx: &Ctx,
) -> Transform {
    const SIGN: [f32; 3] = [0.0, -1.0, 1.0];
    let base = transform;
    for z in 0..3 {
        for x in 0..3 {
            for y in 0..3 {
                let offset = Vec3::new(SIGN[x], SIGN[y], SIGN[z]) * 0.005;
                transform.translation = base.translation + offset;
                let mut free = true;
                spatial.shape_intersections_callback(
                    &ctx.collider,
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
                    return transform;
                }
            }
        }
    }
    base
}

/// Returns the safe hit distance and the hit data from the spatial query.
/// Source: https://github.com/Ploruto/kcc_prototyping/blob/main/src/move_and_slide.rs#L42
#[must_use]
fn sweep_check(
    transform: Transform,
    direction: Dir3,
    max_distance: f32,
    spatial: &SpatialQueryPipeline,
    ctx: &Ctx,
) -> Option<ShapeHitData> {
    let mut hit = spatial.cast_shape(
        &ctx.collider,
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

fn jump_button(velocity: Vec3, grounded: Option<Entity>, ctx: &Ctx) -> (Option<Entity>, Vec3) {
    if !ctx.input.jumped || grounded.is_none() {
        (grounded, velocity)
    } else {
        (None, velocity + Vec3::Y * ctx.cfg.jump_speed)
    }
}
