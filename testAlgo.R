# Create Reward matrix
F_levels = c("zero", "negative_low", "positive_low", "negative_high", "positive_high")
F_values = c(0, -5, 5, -10, 10)
n_actions = length(F_levels)
Reward_matrix = matrix(0, nrow=n_states, ncol=n_actions)
for(i in 1:nrow(Reward_matrix)) {
    v = getState(i)
    if((which(x_levels == v[, 1]) %in% c(1, x_nlevels)) | (which(theta_levels == v[, 3]) %in% c(1, theta_nlevels))) {
        Reward_matrix[i, ] = -10.0
    } else if((which(x_levels == v[, 1]) %in% c((1 + x_nlevels) / 2)) & (which(theta_levels == v[, 3]) %in% c((1 + theta_nlevels) / 2))) {
        Reward_matrix[i, ] = 2.0
    } else {
        Reward_matrix[i, ] = 0.0
    }
}

# Run MDPtoolbox with the transition matrices and reward matrix
library(MDPtoolbox)
neg = read.table("F=-10.0_transition_matrix_0111.csv", header=FALSE, sep=","); neg = as.matrix(neg)
pos = read.table("F=10.0_transition_matrix_0111.csv", header=FALSE, sep=","); pos = as.matrix(pos)
zero = read.table("F=0.0_transition_matrix_0111.csv", header=FALSE, sep=","); zero = as.matrix(zero)
neg_low = read.table("F=-5.0_transition_matrix_0111.csv", header=FALSE, sep=","); neg_low = as.matrix(neg_low)
pos_low = read.table("F=5.0_transition_matrix_0111.csv", header=FALSE, sep=","); pos_low = as.matrix(pos_low)
neg_high = read.table("F=-10.0_transition_matrix_0111.csv", header=FALSE, sep=","); neg_high = as.matrix(neg_high)
pos_high = read.table("F=10.0_transition_matrix_0111.csv", header=FALSE, sep=","); pos_high = as.matrix(pos_high)

T = list(zero=zero, negative=neg, positive=pos)
# T = list(positive=pos, negative=neg)
T = list(zero=zero, negative_low=neg_low, positive_low=pos_low, negative_high=neg_high, positive_high=pos_high)
mdp_check(T, Reward_matrix) # empty string => ok
# m <- mdp_policy_iteration(P=T, R=Reward_matrix, discount=0.8)
# m <- mdp_value_iteration(P=T, R=Reward_matrix, discount=0.8)
m <- mdp_policy_iteration_modified(P=T, R=Reward_matrix, discount=0.9)


# Call python gym environment
library(reticulate)
py_run_string("import gym")
use_python("bin/python")
py_run_string("env = gym.make('CartPole-v0')")
py_run_string("observation = env.reset()")

# while(TRUE)
bad_j = NULL
for (i_try in 1:2) {
    for(j in 1:3500) {
        i = get_discrete_state(py$observation[1], py$observation[2], (py$observation[3] + pi) %% (2 * pi) - pi, py$observation[4])$index
        if(names(T)[m$policy[i]] == "positive_high") {
            action = 10
            py_run_string("observation, reward, done, info = env.step(1)")
        } else if(names(T)[m$policy[i]] == "negative_high") {
            action = -10
            py_run_string("observation, reward, done, info = env.step(0)")
        } else if(names(T)[m$policy[i]] == "positive_low") {
            action = 5
            py_run_string("env.env.force_mag = 5.0")
            py_run_string("observation, reward, done, info = env.step(1)")
            py_run_string("env.env.force_mag = 10.0")
        } else if(names(T)[m$policy[i]] == "negative_low") {
            action = -5
            py_run_string("env.env.force_mag = 5.0")
            py_run_string("observation, reward, done, info = env.step(0)")
            py_run_string("env.env.force_mag = 10.0")
        } else {
            action = 0
            py_run_string("env.env.force_mag = 0.0")
            py_run_string("observation, reward, done, info = env.step(0)")
            py_run_string("env.env.force_mag = 10.0")
        }
        # py_run_string("observation, reward, done, info = env.step(action)")
        py_run_string("env.render()")
        # print(c(py$observation, action))
        # Sys.sleep(0.01)
        if(py$done) {
            bad_j = c(bad_j, j)
            # print(sprintf("FINISHED AFTER %d STEPS", j))
            # break
        }
    }
    py_run_string("env.close()")
    py_run_string("env.reset()")
}

