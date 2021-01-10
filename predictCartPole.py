import numpy as np

F_set = np.array([-10.0, 10.0])
filenames = ["F=" + str(F) + "_transition_matrix.csv" for F in F_set]
P = np.array([np.genfromtxt(file, delimiter=",") for file in filenames])

def policyEvaluation(policy, reward, discount_factor=0.8):
    # Define space with numbers (index)
    action_space = np.arange(policy.shape[0])
    state_space = np.arange(policy.shape[1])

    # Intialize value function
    V = np.zeros(len(state_space))
    # Use V_temp for updating through iteration
    V_temp = V

    # Run loop until convergence
    while True:
        for s in range(len(state_space)):
            # Keep incrementing V_temp[s] using the formula
            V_temp[s] = 0
            for a in range(len(action_space)):
                vvv
