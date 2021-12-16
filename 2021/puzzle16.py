from packet import Packet, setup

def read_input():
    with open('puzzle16.dat') as f:
        encoded = f.read().strip()
        expr = "f'{0x" + encoded + ":0>" + str(len(encoded) * 4) + "b}'"
        data = eval(expr)
        return data

setup(read_input())
packet = Packet()
print (f'Part 1: {packet.version_sum()}')
print (f'Part 2: {packet.value}')
