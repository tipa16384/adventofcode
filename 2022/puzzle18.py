import re
from collections import defaultdict

def read_input():
    "return a list of tuples of 3 numbers"
    with open(r"2022\puzzle18.txt") as f:
        return zip(*[iter(map(int, re.findall(r"\d+", f.read())))]*3)

def face_generator(x, y, z):
    "yield the six faces of a cube"
    yield (x, y, z, x+1, y+1, z)
    yield (x, y, z, x+1, y, z+1)
    yield (x, y, z, x, y+1, z+1)
    yield (x+1, y, z, x+1, y+1, z+1)
    yield (x, y+1, z, x+1, y+1, z+1)
    yield (x, y, z+1, x+1, y+1, z+1)

def today_we_make_faces():
    "set of faces that are only used once"
    faces = defaultdict(int)
    for cube in read_input():
        for face in face_generator(*cube):
            faces[face] += 1
    return set(face for face in faces if faces[face] == 1)

def flood_fill(faces):
    "find the water volume"
    x, y, z, width, height, depth = -1, -1, -1, 23, 23, 23

    water_cubes = set()
    wet_faces = set()
    flood_heap = [(x, y, z)]

    while flood_heap:
        flood = flood_heap.pop()
        if flood not in water_cubes:
            if flood[0] >= -1 and flood[0] <= width and flood[1] >= -1 and flood[1] <= height and flood[2] >= -1 and flood[2] <= depth:
                water_cubes.add(flood)
                deltas = iter([(0, 0, -1), (0, -1, 0), (-1, 0, 0), (1, 0, 0), (0, 1, 0), (0, 0, 1)])
                for face in face_generator(*flood):
                    delta = next(deltas)
                    if face in faces:
                        wet_faces.add(face)
                    else:
                        flood_heap.append(tuple([flood[i] + delta[i] for i in range(3)]))
    return wet_faces

faces = today_we_make_faces()

print("Part 1:", len(faces))
print("Part 2:", len(flood_fill(faces)))
