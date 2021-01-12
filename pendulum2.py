import gym
import time
env = gym.make('CartPole-v0')
for i_episode in range(1):
    observation = env.reset()
    for t in range(500):
        env.render()
        print(observation)
        action = env.action_space.sample()
        if observation[1] >= 0:
            action = 0
        else:
            action = 1
        observation, reward, done, info = env.step(action)
        time.sleep(0.02)
        if done:
            print("Episode finished after {} timesteps".format(t+1))
            # break

env.close()
