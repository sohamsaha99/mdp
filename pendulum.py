import gym
import time
env = gym.make('CartPole-v0')
DEATH = []
for i_episode in range(10):
    observation = env.reset()
    for t in range(200):
        env.render()
        # print(observation)
        action = env.action_space.sample()
        if observation[0] > 0:
            action = 0
        else:
            action = 1
        if observation[2] < 0:
            action = 0
        else:
            action = 1
        observation, reward, done, info = env.step(action)
        time.sleep(0.02)
        if done:
            print("Episode finished after {} timesteps".format(t+1))
            DEATH.append(t+1)
            time.sleep(1.5)
            break
env.close()
