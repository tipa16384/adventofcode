import re

with open('accounting12.dat') as f: data = f.read()

print ("Part 1:", sum(int(x) for x in re.findall(r'\-{,1}\d+', data)))
