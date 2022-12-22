def read_input():
    with open(r"2022\puzzle20.txt") as f:
        return list(enumerate(map(int, f.read().splitlines())))

def index_of_zero(number_list):
    for i in range(len(number_list)):
        if number_list[i][1] == 0:
            return i

def mix(mix_count=1, multiplier=1):
    number_list = read_input()
    list_size = len(number_list)

    number_list = [(i, n * multiplier) for i, n in number_list]

    for _ in range(mix_count):
        for i in range(list_size):
            for j in range(list_size):
                if number_list[j][0] == i:
                    num = number_list[j]
                    number_list.pop(j)
                    if num[1] == -j:
                        number_list.append(num)
                    else:
                        number_list.insert((j + num[1]) % (list_size-1), num)
                    break

    zi = index_of_zero(number_list)
    return sum(number_list[(zi + i) % len(number_list)][1] for i in range(1000, 4000, 1000))

print("Part 1:", mix())
print("Part 2:", mix(10, 811589153))
