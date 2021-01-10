library(MDPtoolbox)

T <- array(0, c(2, 2, 2))
T[,,1] <- matrix(c(0, 1, 0.8, 0.2), nrow=2, ncol=2, byrow=TRUE)
T[,,2] <- matrix(c(0.5, 0.5, 0.1, 0.9), nrow=2, ncol=2, byrow=TRUE)

R <- matrix(c(10, 10, 1, -5), nrow=2, ncol=2, byrow=TRUE)

mdp_check(T, R)

up <- matrix(c( 1, 0, 0, 0,
0.7, 0.2, 0.1, 0,
0, 0.1, 0.2, 0.7,
0, 0, 0, 1),
nrow=4, ncol=4, byrow=TRUE)
left <- matrix(c(0.9, 0.1, 0, 0,
0.1, 0.9, 0, 0,
0, 0.7, 0.2, 0.1,
0, 0, 0.1, 0.9),
nrow=4, ncol=4, byrow=TRUE)
down <- matrix(c(0.3, 0.7, 0, 0,
0, 0.9, 0.1, 0,
0, 0.1, 0.9, 0,
0, 0, 0.7, 0.3),
nrow=4, ncol=4, byrow=TRUE)
right <- matrix(c(0.9, 0.1, 0, 0,
0.1, 0.2, 0.7, 0,
0, 0, 0.9, 0.1,
0, 0, 0.1, 0.9),
nrow=4, ncol=4, byrow=TRUE)

T <- list(up=up, left=left,
down=down, right=right)

R <- matrix(c(-1, -1, -1, -1,
-1, -1, -1, -1,
-1, -1, -1, -1,
10, 10, 10, 10),
nrow=4, ncol=4, byrow=TRUE)

mdp_check(T, R) # empty string => ok

m <- mdp_policy_iteration(P=T, R=R, discount=0.9)

names(T)[m$policy]
