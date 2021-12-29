from re import findall
from json import loads
from multipledispatch import dispatch

@dispatch(int)
def sum_objs(data: int) -> int: return data

@dispatch(str)
def sum_objs(_: str) -> int: return 0

@dispatch(list)
def sum_objs(data: list) -> int: return sum(sum_objs(x) for x in data)

@dispatch(dict)
def sum_objs(data: dict) -> int:
    return 0 if 'red' in data.values() else sum(sum_objs(x) for x in data.values())

with open('accounting12.dat') as f: data = f.read()

print ("Part 1:", sum(int(x) for x in findall(r'\-{,1}\d+', data)))

data = loads(data)

print ("Part 2:", sum_objs(data))
