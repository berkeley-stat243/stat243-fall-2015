## @knitr mutable
l1 = [1, 2]              
t1 = (3, 4)             
d = {"a": l1, "b": t1}

## @knitr mutable1
d
type(d)
type(d["a"])
id(d)

## @knitr mutable2
id(l1)
l1
id(d["a"])
d["a"][1] = 1
d["a"]
l1
id(l1)
id(d["a"])

## @knitr mutable3
d["b"][1] = 1
d["b"] = 1

## @knitr mutable4
e = d
e is d

## @knitr mutable5
e = d.copy()
e is d
e["a"] is d["a"]

## @knitr mutable6
from copy import deepcopy
e = deepcopy(d)
e is d
e["a"] is d["a"]
