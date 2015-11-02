## @knitr float128
import numpy as np

x = np.array([1e-16]*(10001))
x[0]=1
sum(x)
x.sum()
x.sum(dtype=np.float128)

from math import fsum
fsum(x)
