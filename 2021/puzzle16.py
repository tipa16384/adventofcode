from math import prod

operator_fns = {
    0: lambda sp: sum(p.value for p in sp.subpackets),
    1: lambda sp: prod(p.value for p in sp.subpackets),
    2: lambda sp: min(p.value for p in sp.subpackets),
    3: lambda sp: max(p.value for p in sp.subpackets),
    5: lambda sp: 1 if sp.subpackets[0].value > sp.subpackets[1].value else 0,
    6: lambda sp: 1 if sp.subpackets[0].value < sp.subpackets[1].value else 0,
    7: lambda sp: 1 if sp.subpackets[0].value == sp.subpackets[1].value else 0
}

class Packet:
    def __init__(self):
        self.version = self.read_bits(3)
        self.type = self.read_bits(3)
        self.value = 0
        self.subpackets = []

        if self.type == 4:
            self.read_literal()
        else:
            self.read_sub_packets()
            self.compute()
    
    def compute(self):
        self.value = operator_fns[self.type](self)
    
    def read_sub_packets(self):
        global bl, bp
        length_type_id = self.read_bits(1)
        if length_type_id == 0:
            subpacket_length = self.read_bits(15)
            end_p = bp + subpacket_length
            while bp < end_p:
                self.subpackets.append(Packet())
        else:
            num_packets = self.read_bits(11)
            for i in range(num_packets):
                self.subpackets.append(Packet())

    def read_literal(self):
        while True:
            continue_bit = self.read_bits(1)
            self.value = (self.value << 4) + self.read_bits(4)
            if not continue_bit:
                break

    def read_bits(self, n):
        global bp, bl, data
        val = 0
        for i in range(n):
            val = val << 1
            val += ord(data[bp]) - ord('0')
            bp += 1
        return val

    def log(self, indent=0):
        spaces = '  ' * indent
        print (f'{spaces}(Packet version={self.version}, type={self.type}, value={self.value})')
        for subpacket in self.subpackets:
            subpacket.log(indent + 1)

    def version_sum(self):
        return self.version + sum([subpacket.version_sum() for subpacket in self.subpackets])

    def __str__(self):
        return f'(Packet version={self.version}, type={self.type}, value={self.value}, subpackets={self.subpackets})'

    def __repr__(self):
        return self.__str__()

def read_input():
    with open('puzzle16.dat') as f:
        encoded = f.read().strip()
        expr = "f'{0x" + encoded + ":0>" + str(len(encoded) * 4) + "b}'"
        data = eval(expr)
        return data

data = read_input()
bp = 0
bl = len(data)

packet = Packet()
print (f'Part 1: {packet.version_sum()}')
print (f'Part 2: {packet.value}')


