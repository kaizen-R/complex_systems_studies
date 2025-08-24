## "Simple" ODE: (Example 2.2.1 of "Nonlinear Dynamics and Chaos", by S. Strogatz)

## This is just me trying to "wrap my head" around the concepts of ODE.
## Study the "Dynamics", starting as follows:

## The concept of a first order ODE system:
## in here, we consider then, the "x" position is the only one variable 
## that changes with the passing of time:
## I can draw how a particle will move following a line (the base function)
## with goo on it (which makes initial velocity irrelevant), with "potential"
## being here... Gravity in a sense...
## Note: Section 2.7 with the simile of "potentials" and "goo", 
## THAT really helped me understand what we were doing...

## Moreover, I do NOT need the equation (analytical solution) for the base
## function, because I can get a feeling of particle velocity from the 
## vector field.

## But then, I can also integrate numerically, which will help follow
## how a particle following a given function evolves over time...


## Supporting function for today: Draw a Phase Portrait:
draw_phase_portrait <- function(dot_x, x_min=-10, x_max=10, x_step = .01) {
  # if(is.null(dot_x)) return("Single ODE required")
  
  x_range <- seq(x_min, x_max, x_step)
  fun_df <- data.frame(x = x_range, d_x = sapply(x_range, dot_x))
  
  fixed_points_pos <- which(fun_df$d_x==0)
  
  
  
  plot(fun_df, 
       ylab="f(x)",
       type="l", col="blue", main="phase portrait - first order ODE")
  ## Axis help understand the visualization in these cases:
  abline(h=0)
  abline(v=0)
  ## fixed_points:
  if(length(fixed_points_pos) > 0) {
    ## We need to differentiate the STABLE from UNSTABLE points
    ## Stable will be "full points"
    points_types <- rep(1, length(fixed_points_pos))
    # print(points_types)
    for(pos_index in 1:length(fixed_points_pos)) {
      if(fixed_points_pos[pos_index] == 1) { ## i.e. first entry in range:
        if(fun_df[2, 2] < 0) points_types[pos_index] <- 16
      } else {
        if(fun_df[fixed_points_pos[pos_index]-1, 2] > 0) points_types[pos_index] <- 16
      }
    }
    points(fun_df[fixed_points_pos,], pch=points_types) ## That's OK as a start
  }
    
  
}

## Let's use first the Euler method for showing what really happens then:
# x_n+1 = x_n + f(xn)*delta(t), for t very small ideally.
generate_particle_positions_for_x0 <- function(x0, fun_x, n_steps=400, time_step=0.01) {
  particle_positions <- data.frame(t=0, x=x0)
  xn <- x0
  for(i in 1:n_steps) {
    x_n_plus_1 <- xn + dot_x(xn) * time_step
    particle_positions <- 
      rbind(particle_positions,
            data.frame(t=i,x=x_n_plus_1))
    xn <- x_n_plus_1
  }
  particle_positions
}


## For each position, we gather the slope, which is...
## the value of the derivative at that point! Of course.
generate_particle_slopes_for_x0 <- function(x0, dot_x, n_steps=400, time_step=0.01) {
  particle_slopes <- data.frame(t=0, slope=dot_x(x0))
  xn <- x0
  for(i in 1:n_steps) {
    x_n_plus_1 <- xn + dot_x(xn) * time_step
    particle_slopes <- 
      rbind(particle_slopes,
            data.frame(t=i,slope=dot_x(xn)))
    xn <- x_n_plus_1
  }
  particle_slopes
}

## Now to generate the field, we need many "points" to be drawn as small slopes...
generate_time_slopes_field <- function(dot_x, x0_seq=seq(-5, 3, 0.4), time_range=c(0, 4), time_step=0.01) {
  slopes_df <- NULL
  radius <- 0.1
  for(x0 in x0_seq) {
    t_slopes <- generate_particle_slopes_for_x0(x0, dot_x, n_steps=400, time_step = 0.01)
    t_slopes <- t_slopes[1:nrow(t_slopes) %% 15 == 0,]
    if(x0 == 0.9) head(t_slopes)
    for(t_time in 1:nrow(t_slopes)) {
      arrows(radius*cos(atan(t_slopes[t_time, "slope"])+pi)+t_slopes[t_time, "t"],
             radius*sin(atan(t_slopes[t_time, "slope"])+pi)+x0,
             radius*cos(atan(t_slopes[t_time, "slope"]))+t_slopes[t_time, "t"],
             radius*sin(atan(t_slopes[t_time, "slope"]))+x0, 
             length = 0.2*radius)
    }
    
  }
}


## Integrating the example, we are talking about a particle that moves in space
## following:
## f(x) = -1/3x^3+x+C

## original function: Integral of -x^2+1 is:
orig_fun <- function(x, const_C=0) {
  -(x^3)/3+x+const_C
}

## So, derived, we get:
dot_x <- function(x) x^2-1
## Which SHOULD be coherent with ODE:
## d(x)/d(t)-x^2+1=0

par(mfrow=c(2,1)) ## We're going to watch both the function and its ODE

x_min <- -2 ; x_max <- 2

## Visualizing THAT first, you can tell, a particle or liquid around x=-1
## (or to the left of x=1) will tend to stabilize ("fall") towards x=-1 and STOP.
## THAT IS, the particle's VELOCITY at x = -1 will be 0.
## Same for x=1 in fact! But right AROUND x=1, the particle will ACCELERATE
## Because x=1 is UNSTABLE equilibrium.
##
## You can also tell, visually:
## A particle "standing" at 1.1 will fall to the right, accelerating
##
## NOW:
## Suppose Vector field: how fast does a particle move (given by x')?
## x' = x^2-1      (Example 2.2.1)

## I gathered the original function by doing the corresponding transformations:
## Example 2.2.1 becomes:
## d(x)/d(t)-x^2+1=0
## Which tells us, then, by integrating (analytically, that one is easy):
## f(x) = -1/3x^3+x+C

## Graphically, we know that:
## Particle will move slower when x' = 0.

## Fixed points (x'=0) in this case are: x in (-1, 1)
## x=-1 is locally stable equilibrium,
## x=1 is locally unstable equilibrium.
## I'll make sure to distinguish equilibrium types visually as in the book, i.e.
## using "full dots" for stable, "empty circles" for unstable.

## Let's try to draw a "phase portrait" for this x' = f(x)
draw_phase_portrait(dot_x, x_min, x_max)

## Now, clarification: on a phase portrait:
## The "flow" is to the right when dx > 0, to the left when dx < 0.

orig_seq <- seq(x_min, x_max, 0.01)
orig_points <- data.frame(x=orig_seq, y=orig_fun(orig_seq, 0))
plot(orig_points, type="l", main="original function (C=0)")
# abline(h=0)
orig_points[which(round(orig_points$y, 2) == 0),]





## We shall circle back to this value here...
x0 <- (1+sqrt(5))/2
# abline(v=x0, col="grey")
# abline(h=x0, col="grey")


## With fun_x our ODE, we can actually observe behaviour...
## Even without the original integrated function :D
particle_positions1 <- generate_particle_positions_for_x0(-5, fun_x)
particle_positions2 <- generate_particle_positions_for_x0(-2, fun_x)
particle_positions3 <- generate_particle_positions_for_x0(-.5, fun_x)
particle_positions4 <- generate_particle_positions_for_x0(.9, fun_x)
particle_positions5 <- generate_particle_positions_for_x0(1, fun_x)
particle_positions6 <- generate_particle_positions_for_x0(1.1, fun_x)
particle_positions7 <- generate_particle_positions_for_x0(1.2, fun_x)


## SO:
# par(mfrow=c(2,1))

# plot(orig_points, type="l", main="original function (C=0)")
# abline(h=0)
# orig_points[which(round(orig_points$y, 2) == 0),]

par(mfrow=c(1,1))
plot(particle_positions1, 
     ylim = c(-5, 3),
     type="l",
     main="some x0 values in slopes field",
     col="black")
lines(particle_positions2, col="blue")
lines(particle_positions3, col="lightblue")
lines(particle_positions4, col="grey")  
lines(particle_positions5, col="lightgreen")  
lines(particle_positions6, col="green")  
lines(particle_positions7, col="red")  

generate_time_slopes_field(dot_x)
# abline(h=1, lty = 3)

plot(particle_positions4, 
     # ylim = c(-5, 10),
     type="l",
     col="black",
     main="particle position for x0=.9")
orig_fun(1)



## I should stop right there!

## That's coherent, but now something bothers me A LOT!!
## Now, why on EARTH is this all somehow related to the Golden Ratio???
golden_ratio <- (1+sqrt(5))/2
orig_fun(golden_ratio, 0)

## OK, now what would be the behaviour of this ODE for a given x0?
generate_ODE_observations <- function(x0, dot_x, n_steps=10) {
  next_x = dot_x(x0)
  df <- data.frame(x = x0, next_x=next_x)
  
  for(i in 1:(n_steps-1)) {
    new_next_x = round(dot_x(next_x), 10)
    df <- rbind(df,
                data.frame(x = next_x, next_x=new_next_x))
    next_x <- new_next_x
  }
  
  df
}


x0 <- golden_ratio-.001
# x0 <- golden_ratio+.001
final_df <- data.frame(x = x0,
                       y = generate_ODE_observations(x0, dot_x, n_steps = 100))
last_n_steps <- 10
final_df[(nrow(final_df)-last_n_steps):nrow(final_df), ]
