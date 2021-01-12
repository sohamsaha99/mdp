g = 10.0
dt = 0.05
m = 1.0
l = 1.0
max_speed = 8

angle_normalize = function(x) {
    (x + pi) %% (2 * pi) - pi
}

theta_levels = (1:40)
theta_breaks = seq(-180, 180, length.out=41) * pi / 180
theta_dot_levels = (1:16)
theta_dot_breaks = seq(-8, 8, length.out=17)
action_space = seq(-2, 2, length.out=9)

states = paste(c(1:(length(theta_levels) * length(theta_dot_levels))))
actions = as.character(action_space)

get_discrete_state = function(new_theta, new_theta_dot) {
    theta_state = cut(new_theta, breaks=theta_breaks, labels=theta_levels, ordered_result=TRUE, include.lowest=TRUE)
    theta_state = as.numeric(theta_state) - 1
    theta_dot_state = cut(new_theta_dot, breaks=theta_dot_breaks, labels=theta_dot_levels, ordered_result=TRUE, include.lowest=TRUE)
    theta_dot_state = as.numeric(theta_dot_state) - 1
    new_state = theta_state * length(theta_dot_levels) + theta_dot_state + 1
    as.character(new_state)
}

env = function(state, action) {
    state = as.numeric(state)
    action = as.numeric(action)
    state = state - 1
    theta_state = state %/% length(theta_dot_levels) + 1
    theta_range = theta_breaks[theta_state + 0:1]
    theta_dot_state = state %% length(theta_dot_levels) + 1
    theta_dot_range = theta_dot_breaks[theta_dot_state + 0:1]
    rand_theta = runif(1, min=theta_range[1], max=theta_range[2])
    rand_theta_dot = runif(1, min=theta_dot_range[1], theta_dot_range[2])

    cost = ((theta_range[1] + theta_range[2]) / 2) ^ 2 + 0.1 * ((theta_dot_range[1] + theta_dot_range[2]) / 2) ^ 2 + 0.001 * (action ^ 2)
    cost = ((theta_range[1] + theta_range[2]) / 2) ^ 2 #+ 0.1 * ((theta_dot_range[1] + theta_dot_range[2]) / 2) ^ 2 + 0.001 * (action ^ 2)
    new_theta_dot = rand_theta_dot + (-3 * g / (2 * l) * sin(rand_theta + pi) + 3.0 / (m * l ^ 2) * action) * dt
    if(new_theta_dot >= max_speed) {
        new_theta_dot = max_speed
    } else if(new_theta_dot <= -max_speed) {
        new_theta_dot = -max_speed
    }
    new_theta = angle_normalize(rand_theta + new_theta_dot * dt)
    new_state = get_discrete_state(new_theta, new_theta_dot)
    return(list("NextState"=new_state, "Reward"=-cost))
}

library(ReinforcementLearning)

data <- sampleExperience(N = 1000, 
                         env = env, 
                         states = states, 
                         actions = actions)

control <- list(alpha = 0.1, gamma = 0.5, epsilon = 0.1)
model = ReinforcementLearning(data=data, s="State", a="Action", r="Reward", s_new="NextState", iter=5, control=control)

for (i in 1:10) {
    data <- sampleExperience(N = 1000, 
                                 env = env, 
                                 states = states, 
                                 actions = actions, 
                                 actionSelection = "epsilon-greedy",
                                 model = model, 
                                 control = control)

    # Update the existing policy using new training data
    model <- ReinforcementLearning(data, 
                                       s = "State", 
                                       a = "Action", 
                                       r = "Reward", 
                                       s_new = "NextState", 
                                       control = control,
                                       model = model)
}

# Call python gym environment
library(reticulate)
py_run_string("import gym")
use_python("bin/python")
py_run_file("utils.py")
py_run_string("env = gym.make('Pendulum-v0')")
bad_j = NULL
for (i_try in 1:1) {
    py_run_string("observation = env.reset()")
    py_run_string("observation = env.env.state")
    py_run_string("frames = []")
    for(j in 1:500) {
        i = get_discrete_state(angle_normalize(py$observation[1]), py$observation[2])
        # print(py$observation)
        py$action = as.numeric(model$Policy[i])
        # print(i)
        # py_run_string("print(action)")
        py_run_string("observation, reward, done, info = env.step((action, ))")
        py_run_string("observation = env.env.state")
        py_run_string("env.render()")
        Sys.sleep(0.01)
        if(py$done) {
            bad_j = c(bad_j, j)
            # print(sprintf("FINISHED AFTER %d STEPS", j))
            # break
        }
    }
    py_run_string("env.close()")
}
