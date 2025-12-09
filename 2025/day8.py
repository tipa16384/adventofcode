from math import prod

def dist_sq(c1, c2):
    return sum((x - y) ** 2 for x, y in zip(c1, c2))


def read_coords(path: str):
    with open(path) as f:
        return [
            tuple(map(int, line.split(',')))
            for line in f
            if line.strip()
        ]


def main(path="input.txt", num_connections=1000, num_max=3):
    coord_list = read_coords(path)
    circuits = [[coord] for coord in coord_list]

    dist_list = sorted(
        (dist_sq(c1, c2), c1, c2)
        for i, c1 in enumerate(coord_list)
        for c2 in coord_list[i + 1 :]
    )

    def circuit_containing(coord):
        for circuit in circuits:
            if coord in circuit:
                return circuit
        return None

    con_cnt = 0
    for _, c1, c2 in dist_list:
        circ1 = circuit_containing(c1)
        circ2 = circuit_containing(c2)
        if circ1 is not circ2:
            if len(circuits) == 2:
                print("Part 2", c1[0] * c2[0])
                break
            circ1.extend(circ2)
            circuits.remove(circ2)

        con_cnt += 1
        if con_cnt == num_connections:
            circuits.sort(key=len, reverse=True)
            print("Part 1", prod(len(c) for c in circuits[:num_max]))


if __name__ == "__main__":
    main(num_connections=10)
