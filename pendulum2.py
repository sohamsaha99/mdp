import gym
import time
env = gym.make('CartPole-v0')
for i_episode in range(1):
    observation = env.reset()
    for t in range(250):
        env.render()
        print(observation)
        action = env.action_space.sample()
        if observation[2] >= 0:
            action = 1
        else:
            action = 0
        observation, reward, done, info = env.step(action)
        time.sleep(0.05)
        if done:
            print("Episode finished after {} timesteps".format(t+1))
            # break

env.close()
