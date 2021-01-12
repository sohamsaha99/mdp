# Define constants
g = 9.8
m = 0.1
M = 1.0
L = 1.0
tau = 0.02

# Define Levels with orders
# x_levels = c("Left-Forbidden", "Left-Red", "Left-Yellow", "Left-Green", "Right-Green", "Right-Yellow", "Right-Red", "Right-Forbidden")
# x_breaks = c(-3.2, -2.4, -1.6, -0.8, 0.0, 0.8, 1.6, 2.4, 3.2)
# x_levels = c("Left-Forbidden", "Left-Bad", "Good", "Right-Bad", "Right-Forbidden")
# x_breaks = c(-3.2, -2.4, -0.8, 0.8, 2.4, 3.2)
x_levels = c("Left-Forbidden", "Good", "Right-Forbidden")
x_breaks = c(-2.7, -2.0, 2.0, 2.7)
# v_levels = c("Left-High", "Left-Low", "Right-Low", "Right-High")
# v_breaks = c(-1.0, -0.5, 0.0, 0.5, 1.0)
v_levels = c("Left-High", "Left-Low", "Neutral", "Right-Low", "Right-High")
v_breaks = c(-1.5, -0.9, -0.3, 0.3, 0.9, 1.5)
# theta_levels = c("Left-Forbidden", "Left-Red", "Left-Yellow", "Left-Green", "Right-Green", "Right-Yellow", "Right-Red", "Right-Forbidden")
# theta_breaks = c(-16.0, -12.0, -8.0, -4.0, 0.0, 4.0, 8.0, 12.0, 16.0) * pi / 180
theta_levels = c("Left-Forbidden", "Left-Bad", "Good", "Right-Bad", "Right-Forbidden")
# theta_breaks = c(-24.0, -12.0, -4.0, 4.0, 12.0, 24.0) * pi / 180
theta_breaks = c(-14.0, -8.4, -2.8, 2.8, 8.4, 14.0) * pi / 180
# theta_dot_levels = c("Left-High", "Left-Low", "Right-Low", "Right-High")
# theta_dot_breaks = c(-1.0, -0.5, 0.0, 0.5, 1.0)# * pi / 180
theta_dot_levels = c("Left-High", "Left-Low", "Neutral", "Right-Low", "Right-High")
theta_dot_breaks = c(-1.5, -0.9, -0.3, 0.3, 0.9, 1.5)
x_nlevels = length(x_levels); x_breaks_min = min(x_breaks); x_breaks_max = max(x_breaks)
v_nlevels = length(v_levels); v_breaks_min = min(v_breaks); v_breaks_max = max(v_breaks)
theta_nlevels = length(theta_levels); theta_breaks_min = min(theta_breaks); theta_breaks_max = max(theta_breaks)
theta_dot_nlevels = length(theta_dot_levels); theta_dot_breaks_min = min(theta_dot_breaks); theta_dot_breaks_max = max(theta_dot_breaks)

# Define count matrix
n_states = x_nlevels * v_nlevels * theta_nlevels * theta_dot_nlevels
# count = matrix(0, nrow=n_states, ncol=n_states)

getState = function(x) {
    x = x - 1
    a = x %/% (v_nlevels * theta_nlevels * theta_dot_nlevels)
    a = x_levels[a+1]
    x = x %% (v_nlevels * theta_nlevels * theta_dot_nlevels)
    b = x %/% (theta_nlevels * theta_dot_nlevels)
    b = v_levels[b+1]
    x = x %% (theta_nlevels * theta_dot_nlevels)
    c = x %/% theta_dot_nlevels
    c = theta_levels[c+1]
    x = x %% theta_dot_nlevels
    d = x
    d = theta_dot_levels[d+1]
    M = matrix(c(a, b, c, d), ncol=4)
    colnames(M) = c("x", "v", "theta", "theta_dot")
    data.frame(M)
}

# Given continuous input, find the category
get_discrete_state = function(x, v, theta, theta_dot) {
    # For each variable, find the appropriate interval
    # and keep updating the index in collection of all possible combinations

    if(x < x_breaks_min) {
        x = x_breaks_min
    } else if (x > x_breaks_max) {
        x = x_breaks_max
    }
    x_state = cut(x, breaks=x_breaks, include.lowest=TRUE, labels=x_levels, ordered_result=TRUE)
    index = as.numeric(x_state) - 1
    x_state = as.character(x_state)

    if (v < v_breaks_min) {
        v = v_breaks_min
    } else if (v > v_breaks_max) {
        v = v_breaks_max
    }
    v_state = cut(v, breaks=v_breaks, include.lowest=TRUE, labels=v_levels, ordered_result=TRUE)
    index = v_nlevels * index + as.numeric(v_state) - 1
    v_state = as.character(v_state)

    if (theta < theta_breaks_min) {
        theta = theta_breaks_min
    } else if (theta > theta_breaks_max) {
        theta = theta_breaks_max
    }
    theta_state = cut(theta, breaks=theta_breaks, include.lowest=TRUE, labels=theta_levels, ordered_result=TRUE)
    index = theta_nlevels * index + as.numeric(theta_state) - 1
    theta_state = as.character(theta_state)

    if (theta_dot < theta_dot_breaks_min) {
        theta_dot = theta_dot_breaks_min
    } else if (theta_dot > theta_dot_breaks_max) {
        theta_dot = theta_dot_breaks_max
    }
    theta_dot_state = cut(theta_dot, breaks=theta_dot_breaks, include.lowest=TRUE, labels=theta_dot_levels, ordered_result=TRUE)
    index = theta_dot_nlevels * index + as.numeric(theta_dot_state) - 1
    theta_dot_state = as.character(theta_dot_state)

    # 1-based indexing in R
    index = index + 1

    list(description=c(x_state, v_state, theta_state, theta_dot_state), index=index)
}

# transitionMatrix = function(F, x_low, x_high, v_low, v_high, theta_low, theta_high, theta_dot_low, theta_dot_high) {
count = matrix(0, nrow=n_states, ncol=n_states)
transitionMatrix = function(F, x_state, v_state, theta_state, theta_dot_state, h_x=8, h_v=8, h_theta=8, h_theta_dot=8) {
    # Define boundary from state name
    p = which(x_levels == x_state)
    x_low = x_breaks[p]; x_high = x_breaks[p+1]
    p = which(v_levels == v_state)
    v_low = v_breaks[p]; v_high = v_breaks[p+1]
    p = which(theta_levels == theta_state)
    theta_low = theta_breaks[p]; theta_high = theta_breaks[p+1]
    p = which(theta_dot_levels == theta_dot_state)
    theta_dot_low = theta_dot_breaks[p]; theta_dot_high = theta_dot_breaks[p+1]
    print(c(x_low, x_high, v_low, v_high, theta_low, theta_high, theta_dot_low, theta_dot_high))

    # Define count matrix
    n_states = x_nlevels * v_nlevels * theta_nlevels * theta_dot_nlevels
    count[] = 0

    # Modify boundaries to avoid overlap
    eps = .Machine$double.eps * 1000
    x_low = x_low + eps
    x_high = x_high - eps
    v_low = v_low + eps
    v_high = v_high - eps
    theta_low = theta_low + eps
    theta_high = theta_high - eps
    theta_dot_low = theta_dot_low + eps
    theta_dot_high = theta_dot_high - eps

    # Sets to perform iteration
    x_set = seq(x_low, x_high, length.out=h_x)
    v_set = seq(v_low, v_high, length.out=h_v)
    theta_set = seq(theta_low, theta_high, length.out=h_theta)
    theta_dot_set = seq(theta_dot_low, theta_dot_high, length.out=h_theta_dot)

    # print(c(length(x_set), length(v_set), length(theta_set), length(theta_dot_set)))

    # Iteration over values for the provided discrete state
    for (i in 1:length(x_set)) {
        for (j in 1:length(v_set)) {
            for (k in 1:length(theta_set)) {
                for (l in 1:length(theta_dot_set)) {

                    # Define the exact state
                    x = x_set[i]
                    v = v_set[j]
                    theta = theta_set[k]
                    theta_dot = theta_dot_set[l]

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
# F = 10.0
# eps = .Machine$double.eps
# P = transitionMatrix(F=0.0, x_low=-1.6+eps, x_high=-0.8-eps, v_low=0.0+eps, v_high=0.5-eps, theta_low=-8*pi/180+eps, theta_high=-4*pi/180-eps, theta_dot_low=-0.5+eps, theta_dot_high=0.0-eps)
# P = transitionMatrix(F=F, x_state="Right-Forbidden", v_state="Right-High", theta_state="Good", theta_dot_state="Right-High", h=6)

# u = rowSums(P)
# print(cbind(getState(which(u != 0)), u[u != 0]))
# v = colSums(P)
# print(cbind(getState(which(v != 0)), v[v != 0] / sum(P)))
# Q = P / rowSums(P)
# Q[is.nan(Q)] = 0.0
# write.table(Q, file=paste0("F=", sprintf("%.1f", F), "_transition_matrix.csv"), sep=",", col.names=FALSE, row.names=FALSE)


# Define Transition matrix
n_states = x_nlevels * v_nlevels * theta_nlevels * theta_dot_nlevels
P_matrix = matrix(0, nrow=n_states, ncol=n_states)

fullTransitionMatrix = function(F) {
    P_matrix[] = 0
    for (x in x_levels) {
        for (v in v_levels) {
            for (theta in theta_levels) {
                for (theta_dot in theta_dot_levels) {
                    P = transitionMatrix(F=F, x_state=x, v_state=v, theta_state=theta, theta_dot_state=theta_dot, h_x=8, h_v=6, h_theta=8, h_theta_dot=6)
                    Q = P / rowSums(P)
                    Q[is.nan(Q)] = 0.0
                    P_matrix = P_matrix + Q
                }
            }
        }
    }
    write.table(P_matrix, file=paste0("F=", sprintf("%.1f", F), "_transition_matrix_0111.csv"), sep=",", col.names=FALSE, row.names=FALSE)
}

# write.table(P_matrix, file=paste0("F=", sprintf("%.1f", F), "_transition_matrix.csv"), sep=",", col.names=FALSE, row.names=FALSE)

# Create Reward matrix
F_levels = c("negative", "positive")
F_values = c(-10, 10)
n_actions = length(F_levels)
Reward_matrix = matrix(0, nrow=n_states, ncol=n_actions)
for(i in 1:nrow(Reward_matrix)) {
    v = getState(i)
    if((which(x_levels == v[, 1]) %in% c(1, x_nlevels)) | (which(theta_levels == v[, 3]) %in% c(1, theta_nlevels))) {
        Reward_matrix[i, ] = -10.0
    } else {
        Reward_matrix[i, ] = 1.0
    }
}

# # Run MDPtoolbox with the transition matrices and reward matrix
# library(MDPtoolbox)
# neg = read.table("F=-10.0_transition_matrix.csv", header=FALSE, sep=","); neg = as.matrix(neg)
# pos = read.table("F=10.0_transition_matrix.csv", header=FALSE, sep=","); pos = as.matrix(pos)
# T = list(negative=neg, positive=pos)
# mdp_check(T, Reward_matrix) # empty string => ok
# m <- mdp_policy_iteration(P=T, R=Reward_matrix, discount=0.9)


# # Call python gym environment
# library(reticulate)
# py_run_string("import gym")
# use_python("bin/python")
# py_run_string("env = gym.make('CartPole-v0')")
# py_run_string("observation = env.reset()")

# # while(TRUE)
# for(j in 1:250) {
#     i = get_discrete_state(py$observation[1], py$observation[2], py$observation[3], py$observation[4])$index
#     if(names(T)[m$policy[i]] == "positive") {
#         action = 1
#         py_run_string("observation, reward, done, info = env.step(1)")
#     } else {
#         action = 0
#         py_run_string("observation, reward, done, info = env.step(0)")
#     }
#     # py_run_string("observation, reward, done, info = env.step(action)")
#     py_run_string("env.render()")
#     print(c(py$observation, action))
#     Sys.sleep(0.05)
#     if(py$done) {
#         print("WARNING: FINISHED")
#         # break
#     }
# }
# py_run_string("env.close()")
