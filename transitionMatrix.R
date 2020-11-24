# Define constants
g = 9.8
m = 0.1
M = 1.0
L = 1.0
tau = 0.2

# Define Levels with orders
x_levels = c("Left-Forbidden", "Left-Red", "Left-Yellow", "Left-Green", "Right-Green", "Right-Yellow", "Right-Red", "Right-Forbidden")
v_levels = c("Left-High", "Left-Low", "Right-Low", "Right-High")
theta_levels = c("Left-Forbidden", "Left-Red", "Left-Yellow", "Left-Green", "Right-Green", "Right-Yellow", "Right-Red", "Right-Forbidden")
theta_dot_levels = c("Left-High", "Left-Low", "Right-Low", "Right-High")
x_nlevels = length(x_levels)
v_nlevels = length(v_levels)
theta_nlevels = length(theta_levels)
theta_dot_nlevels = length(theta_dot_levels)

# Define count matrix
n_states = x_nlevels * v_nlevels * theta_nlevels * theta_dot_nlevels
count = matrix(0, nrow=n_states, ncol=n_states)

# Given continuous input, find the category
get_discrete_state = function(x, v, theta, theta_dot) {
    # States for position:
    # [-3.2, -2.4), [-2.4, -1.6), [-1.6, -0.8), [-0.8, 0.0), [0.0, 0.8), [0.8, 1.6), [1.6, 2.4), [2.4, 3.2)
    # Left-Forbidden, Left-Red, Left-Yellow, Left-Green, Right-Green, Right-Yellow, Right-Red, Right-Forbidden
    if (x < -2.4) {
        x_state = "Left-Forbidden"
    } else if (x >= -2.4 & x < -1.6) {
        x_state = "Left-Red"
    } else if (x >= -1.6 & x < -0.8) {
        x_state = "Left-Yellow"
    } else if (x >= -0.8 & x < 0.0) {
        x_state = "Left-Green"
    } else if (x >= 0.0 & x < 0.8) {
        x_state = "Right-Green"
    } else if (x >= 0.8 & x < 1.6) {
        x_state = "Right-Yellow"
    } else if (x >= 1.6 & x < 2.4) {
        x_state = "Right-Red"
    } else if (x >= 2.4) {
        x_state = "Right-Forbidden"
    }

    # States for angle
    # [-16, -12), [-12, -8), [-8, -4), [-4, 0.0), [0.0, 4), [4, 8), [8, 12), [12, 16)
    # Left-Forbidden, Left-Red, Left-Yellow, Left-Green, Right-Green, Right-Yellow, Right-Red, Right-Forbidden
    theta = theta * 180 / pi
    if (theta < -12) {
        theta_state = "Left-Forbidden"
    } else if (theta >= -12 & theta < -8) {
        theta_state = "Left-Red"
    } else if (theta >= -8 & theta < -4) {
        theta_state = "Left-Yellow"
    } else if (theta >= -4 & theta < 0) {
        theta_state = "Left-Green"
    } else if (theta >= 0 & theta < 4) {
        theta_state = "Right-Green"
    } else if (theta >= 4 & theta < 8) {
        theta_state = "Right-Yellow"
    } else if (theta >= 8 & theta < 12) {
        theta_state = "Right-Red"
    } else if (theta >= 12) {
        theta_state = "Right-Forbidden"
    }

    # States for velocity
    # [-1.0, -0.5), [-0.5, 0.0), [0.0, 0.5), [0.5, 1.0)
    # Left-High, Left-Low, Right-Low, Right-High
    if (v < -0.5) {
        v_state = "Left-High"
    } else if (v >= -0.5 & v < 0.0) {
        v_state = "Left-Low"
    } else if (v >= 0.0 & v < 0.5) {
        v_state = "Right-Low"
    } else if (v >= 0.5) {
        v_state = "Right-High"
    }

    # States for angular velocity
    # [-1.0, -0.5), [-0.5, 0.0), [0.0, 0.5), [0.5, 1.0)
    # Left-High, Left-Low, Right-Low, Right-High
    if (theta_dot < -0.5) {
        theta_dot_state = "Left-High"
    } else if (theta_dot >= -0.5 & theta_dot < 0.0) {
        theta_dot_state = "Left-Low"
    } else if (theta_dot >= 0.0 & theta_dot < 0.5) {
        theta_dot_state = "Right-Low"
    } else if (theta_dot >= 0.5) {
        theta_dot_state = "Right-High"
    }

    # Get index in set of all combinations
    index = as.numeric(factor(x_state, levels=x_levels, ordered=TRUE)) - 1
    index = v_nlevels * index + as.numeric(factor(v_state, levels=v_levels, ordered=TRUE)) - 1
    index = theta_nlevels * index + as.numeric(factor(theta_state, levels=theta_levels, ordered=TRUE)) - 1
    index = theta_dot_nlevels * index + as.numeric(factor(theta_dot_state, levels=theta_dot_levels, ordered=TRUE)) - 1
    index = index + 1
    list(description=c(x_state, v_state, theta_state, theta_dot_state) ,index=index)
}

transitionMatrix = function(F, x_low, x_high, v_low, v_high, theta_low, theta_high, thetadot_low, thetadot_high) {
    # Sets to perform iteration
    x_set = seq(x_low, x_high, by=0.05)
    v_set = seq(v_low, v_high, by=0.05)
    theta_set = seq(theta_low, theta_high, by=0.05)
    thetadot_set = seq(thetadot_low, thetadot_high, by=0.05)

    # Iteration over values for the provided discrete state
    for (i in 1:length(x_set)) {
        for (j in 1:length(v_set)) {
            for (k in 1:length(theta_set)) {
                for (l in 1:length(thetadot_set)) {

                    # Define the exact state
                    x = x_set[i]
                    v = v_set[j]
                    theta = theta_set[k]
                    theta_dot = thetadot_set[l]

                    # Get discrete state
                    state = get_discrete_state(x, v, theta, theta_dot)
                    index = state$index
                    state = state$description

                    # Formula for acceleration
                    temp = (F + 0.5 * m * L * (theta_dot ^ 2) * sin(theta)) / (m + M)
                    theta_acc = (g * sin(theta) - temp * cos(theta)) / (0.5 * L * (4.0 / 3.0 - m * (cos(theta) ^ 2 / (m + M))))
                    x_acc = temp - 0.5 * m * L * theta_acc * cos(theta) / (m + M)

                    # Update the state using Euler's formula
                    x = x + tau * v
                    v = v + tau * x_acc
                    theta = theta + tau * theta_dot
                    theta_dot = theta_dot + tau * theta_acc

                    # Obtain updated discrete state
                    updated_state = get_discrete_state(x, v, theta, theta_dot)
                    updated_index = updated_state$index
                    updated_state = updated_state$description

                    # Update the count matrix
                    count[index, updated_index] = count[index, updated_index] + 1
                }
            }
        }
    }
    count
}

# Example of function call
eps = .Machine$double.eps
P = transitionMatrix(F=10, x_low=-1.6+eps, x_high=-0.8-eps, v_low=0.0+eps, v_high=0.5-eps, theta_low=-8*pi/180+eps, theta_high=-4*pi/180-eps, thetadot_low=-0.5+eps, thetadot_high=0.0-eps)
