## @knitr p1
import numpy as np
import matplotlib.pyplot as plt

np.random.seed(2)

code  = {"up": (0,1),
         "down": (0,-1),
         "left": (-1,0),
         "right": (1,0)}

def random_2d_walk(nsteps=100):
    steps = np.random.choice(code.keys(), nsteps)
    walk = np.array([code[step] for step in steps])
    xy = walk.cumsum(axis=0)
    return (xy, walk)

xy, walk = random_2d_walk()
plt.plot(xy[:,0], xy[:,1])
# plt.savefig("test.png")

## @knitr p1.a
def random_2d_walk(nsteps=100):
    if not isinstance(nsteps, int) or nsteps < 1:
        raise ValueError("Number of steps must be a positive integer.")
    ...

## @knitr p2
import numpy as np
import matplotlib.pyplot as plt

class Random2DWalk(object):

    def __init__(self, start=(0,0)):
        self.steps = ["start"]
        self.walk = np.array([start])
        self._code = {"up": (0,1),
                     "down": (0,-1),
                     "left": (-1,0),
                     "right": (1,0)}

    def __str__(self):
        current_position = self.position()[-1]
        total_steps = len(self.steps) - 1
        message = "After {0} steps you are at position: {1}"
        return message.format(total_steps, current_position)

    def step(self, nsteps=100):
        steps = np.random.choice(self._code.keys(), nsteps)
        walk = np.array([self._code[step] for step in steps])
        self.steps += steps
        self.walk = np.vstack([self.walk, walk])
        return None

    def position(self):
        return self.walk.cumsum(axis=0)

    def plot(self):
        xy = self.position()
        plt.plot(xy[:,0], xy[:,1])
        plt.savefig("test.png")
        return None

np.random.seed(2)
rw = Random2DWalk()
print rw
rw.step()
print rw
print rw.steps[:5]
print rw.walk[:5]
rw.plot()
