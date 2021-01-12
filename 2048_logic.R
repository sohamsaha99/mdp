target = 32
n_states = (log(target, base=2) + 1) ^ 4
n_actions = 4

getState = function(x) {
    x = x - 1
    k = log(target, base=2) + 1
    d = x %% k
    x = x %/% k
    c = x %% k 
    x = x %/% k
    b = x %% k
    x = x %/% k
    a = x %% k

    2 ^ c(a, b, c, d)
}

encodeState = function(v) {
    a = v[1]; b = v[2]; c = v[3]; d = v[4]
    a = log(a, base=2)
    b = log(b, base=2)
    c = log(c, base=2)
    d = log(d, base=2)
    k = log(target, base=2) + 1

    min(n_states, 1 + d + k * c + k^2 * b + k^3 * a)
}

### v = c(a, b, c, d)
### a b
### c d
moveRight = function(v) {
    a = v[1]; b = v[2]; c = v[3]; d = v[4]
    movement = FALSE # Flag to check if there is any effect of the move
    if(b == 1) {
        b_new = a
        a_new = 1
        if(a > 1) {
            movement = TRUE
        }
    } else if(b == a) {
        b_new = a + b
        a_new = 1
        movement = TRUE
    } else {
        b_new = b
        a_new = a
    }
    if(d == 1) {
        d_new = c
        c_new = 1
        if(c > 1){
            movement = TRUE
        }
    } else if(d == c) {
        d_new = c + d
        c_new = 1
        movement = TRUE
    } else {
        d_new = d
        c_new = c
    }
    v = c(a_new, b_new, c_new, d_new)
    # print(movement)
    n_empty = sum(v == 1)

    # If there is not any movement, the move is not valid
    if(!movement) {
        # print(11)
        return(list(nextStates=list(rep(1, 4)), probs=c(1)))
        # return(list(nextStates=list(v), probs=c(1)))
        # print(22)
    }
    if(n_empty == 0) {
        return(list(nextStates=list(v), probs=c(1)))
    }

    i = 0
    l = list()
    prob = NULL
    if(a_new == 1) {
        i = i + 1
        l[[i]] = c(2, v[2:4])
        prob[i] = 0.9 / n_empty

        i = i + 1
        l[[i]] = c(4, v[2:4])
        prob[i] = 0.1 / n_empty
    }
    if(b_new == 1) {
        i = i + 1
        l[[i]] = c(v[1], 2, v[3:4])
        prob[i] = 0.9 / n_empty

        i = i + 1
        l[[i]] = c(v[1], 4, v[3:4])
        prob[i] = 0.1 / n_empty
    }
    if(c_new == 1) {
        i = i + 1
        l[[i]] = c(v[1:2], 2, v[4])
        prob[i] = 0.9 / n_empty

        i = i + 1
        l[[i]] = c(v[1:2], 4, v[4])
        prob[i] = 0.1 / n_empty
    }
    if(d_new == 1) {
        i = i + 1
        l[[i]] = c(v[1:3], 2)
        prob[i] = 0.9 / n_empty

        i = i + 1
        l[[i]] = c(v[1:3], 4)
        prob[i] = 0.1 / n_empty
    }
    return(list(nextStates=l, probs=prob))
}

## From 
## a b
## c d to
##
## c a
## d b
rotateClockwise = function(v) {
    v[c(3, 1, 4, 2)]
}

moveLeft = function(v) {
    v = rotateClockwise(rotateClockwise(v))
    output = moveRight(v)
    l = list()
    probs = NULL
    i = 0
    for (j in 1:length(output$nextStates)) {
        i = i + 1
        l[[i]] = rotateClockwise(rotateClockwise(output$nextStates[[j]]))
        probs[[i]] = output$probs[[j]]
    }
    list(nextStates=l, probs=probs)
}

moveUp = function(v) {
    v = rotateClockwise(v)
    output = moveRight(v)
    l = list()
    probs = NULL
    i = 0
    for (j in 1:length(output$nextStates)) {
        i = i + 1
        l[[i]] = rotateClockwise(rotateClockwise(rotateClockwise(output$nextStates[[j]])))
        probs[[i]] = output$probs[[j]]
    }
    list(nextStates=l, probs=probs)
}

moveDown = function(v) {
    v = rotateClockwise(rotateClockwise(rotateClockwise(v)))
    output = moveRight(v)
    l = list()
    probs = NULL
    i = 0
    for (j in 1:length(output$nextStates)) {
        i = i + 1
        l[[i]] = rotateClockwise(output$nextStates[[j]])
        probs[[i]] = output$probs[[j]]
    }
    list(nextStates=l, probs=probs)
}

## Create transition matrix
P_right = matrix(0, nrow=n_states, ncol=n_states)
for (i in 1:nrow(P_right)) {
    output = moveRight(getState(i))
    nextStates = output$nextStates
    probs = output$probs
    for (j in 1:length(probs)) {
        P_right[i, encodeState(nextStates[[j]])] = probs[j]
    }
}
P_left = matrix(0, nrow=n_states, ncol=n_states)
for (i in 1:nrow(P_left)) {
    output = moveLeft(getState(i))
    nextStates = output$nextStates
    probs = output$probs
    for (j in 1:length(probs)) {
        P_left[i, encodeState(nextStates[[j]])] = probs[j]
    }
}
P_up = matrix(0, nrow=n_states, ncol=n_states)
for (i in 1:nrow(P_up)) {
    output = moveUp(getState(i))
    nextStates = output$nextStates
    probs = output$probs
    for (j in 1:length(probs)) {
        P_up[i, encodeState(nextStates[[j]])] = probs[j]
    }
}
P_down = matrix(0, nrow=n_states, ncol=n_states)
for (i in 1:nrow(P_down)) {
    output = moveDown(getState(i))
    nextStates = output$nextStates
    probs = output$probs
    for (j in 1:length(probs)) {
        P_down[i, encodeState(nextStates[[j]])] = probs[j]
    }
}

Reward = matrix(0, nrow=n_states, ncol=n_actions)
Reward[1, ] = -5.0
for(i in 1:nrow(Reward)) {
    if(32 %in% getState(i)) {
        Reward[i, ] = 5.0
    }
}

library(MDPtoolbox)
T = list("DOWN"=P_down, "LEFT"=P_left, "RIGHT"=P_right, "UP"=P_up)
m = mdp_value_iteration(P=T, R=Reward, discount=0.9)

library(reticulate)
use_python("bin/python")
py_run_file("2048_website.py")
py_run_string("status = getBoardStatus(htmlElem)")
current_state = encodeState(py$status)
for (i in 1:100) {
    old_state = current_state
    action = names(T)[m$policy[current_state]]
    print(action)
    py_run_string(paste0("htmlElem.send_keys(Keys.", action, ")"))
    Sys.sleep(1)
    py_run_string("status = getBoardStatus(htmlElem)")
    current_state = encodeState(py$status)
    if(old_state == current_state) {
        print("FINISHED")
        break
    }
}
Sys.sleep(8)
py_run_string("driver.close()")
