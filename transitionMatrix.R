# Define constants
g = 9.8
m = 0.1
M = 1.0
L = 1.0
tau = 0.2

get_discrete_state = function(x, v, theta, theta_dot) {
    # States for position:
    # [-3.2, -2.4), [-2.4, -1.6), [-1.6, -0.8), [-0.8, 0.0), [0.0, 0.8), [0.8, 1.6), [1.6, 2.4), [2.4, 3.2)
    # Low-Forbidden, Low-Red, Low-Yellow, Low-Green, High-Green, High-Yellow, High-Red, High-Forbidden
    if (x < 2.4) {
        x_state = "Low-Forbidden"
    } else if (x >= -2.4 & x < -1.6) {
        x_state = "Low-Red"
    } else if (x >= -1.6 & x < -0.8) {
        x_state = "Low-Yellow"
    } else if (x >= -0.8 & x < 0.0) {
        x_state = "Low-Green"
    } else if (x >= 0.0 & x < 0.8) {
        x_state = "High-Green"
    } else if (x >= 0.8 & x < 1.6) {
        x_state = "High-Yellow"
    } else if (x >= 1.6 & x < 2.4) {
        x_state = "High-Red"
    } else if (x >= 2.4) {
        x_state = "High-Forbidden"
    }

    # States for angle
    # [-16, -12), [-12, -8), [-8, -4), [-4, 0.0), [0.0, 4), [4, 8), [8, 12), [12, 16)
    # Low-Forbidden, Low-Red, Low-Yellow, Low-Green, High-Green, High-Yellow, High-Red, High-Forbidden
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
    # [-3.2, -2.4), [-2.4, -1.6), [-1.6, -0.8), [-0.8, 0.0), [0.0, 0.8), [0.8, 1.6), [1.6, 2.4), [2.4, 3.2)
    # Low-Forbidden, Low-Red, Low-Yellow, Low-Green, High-Green, High-Yellow, High-Red, High-Forbidden
    if (x < 2.4) {
        x_state = "Left-Forbidden"
    } else if (x >= -2.4 & x < -1.6) {
        x_state = "Left-Red"
    } else if (x >= -1.6 & x < -0.8) {
        x_state = "Left-Yellow"
    } else if (x >= -0.8 & x < 0.0) {
        x_state = "Low-Green"
    } else if (x >= 0.0 & x < 0.8) {
        x_state = "Right-Green"
    } else if (x >= 0.8 & x < 1.6) {
        x_state = "Right-Yellow"
    } else if (x >= 1.6 & x < 2.4) {
        x_state = "Right-Red"
    } else if (x >= 2.4) {
        x_state = "Right-Forbidden"
    }
}

function(F, x_low, x_high, v_low, v_high, theta_low, theta_high, thetadot_low, thetadot_high) {
    # Sets to perform iteration
    x_set = seq(x_low, x_high, step=0.01)
    v_set = seq(v_low, v_high, step=0.01)
    theta_set = seq(theta_low, theta_high, step=0.01)
    thetadot_set = seq(thetadot_low, thetadot_high, step=0.01)

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

                    # Formula for acceleration
                    temp = (F + 0.5 * m * L * (theta_dot ^ 2) * sin(theta)) / (m + M)
                    theta_acc = (g * sin(theta) - temp * cos(theta)) / (0.5 * L * (4.0 / 3.0 - m * (cos(theta) ^ 2 / (m + M))))
                    x_acc = temp - 0.5 * m * L * theta_acc * cos(theta) / (m + M)

                    # Update the state using Euler's formula
                    x = x + tau * v
                    v = v + tau * x_acc
                    theta = theta + tau * theta_dot
                    theta_dot = theta_dot + tau * theta_acc

                    # Obtain discrete state
                    updated_state = get_discrete_state(x, v, theta, theta_dot)
                }
            }
        }
    }
}