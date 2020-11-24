import math
import gym
from gym import spaces, logger
from gym.utils import seeding
import numpy as np

class playerCartPoleCalculation:
    """This class is for predicting next state given the current state and action"""
    def __init__(self, arg):
        super(playerCartPoleCalculation, self).__init__()
        self.arg = arg
        