## @knitr myfunc
def myfunc(x):
    if not x in myfunc.table:
        myfunc.table[x] = x*10
    return myfunc.table[x]

## @knitr myfunc1
AttributeError: 'function' object has no attribute 'table'

## @knitr myfunc2
myfunc.table = {}

## @knitr myfunc3
type(myfunc)
type(myfunc.table)
myfunc.table
myfunc(2)
myfunc.table

