"""
███████╗ ██████╗ █████╗ ███╗   ██╗███╗   ██╗███████╗██████╗ ███████╗           
██╔════╝██╔════╝██╔══██╗████╗  ██║████╗  ██║██╔════╝██╔══██╗██╔════╝           
███████╗██║     ███████║██╔██╗ ██║██╔██╗ ██║█████╗  ██████╔╝███████╗           
╚════██║██║     ██╔══██║██║╚██╗██║██║╚██╗██║██╔══╝  ██╔══██╗╚════██║           
███████║╚██████╗██║  ██║██║ ╚████║██║ ╚████║███████╗██║  ██║███████║           
╚══════╝ ╚═════╝╚═╝  ╚═╝╚═╝  ╚═══╝╚═╝  ╚═══╝╚══════╝╚═╝  ╚═╝╚══════╝           
                                                                               
██╗     ██╗██╗   ██╗███████╗    ██╗███╗   ██╗    ██╗   ██╗ █████╗ ██╗███╗   ██╗
██║     ██║██║   ██║██╔════╝    ██║████╗  ██║    ██║   ██║██╔══██╗██║████╗  ██║
██║     ██║██║   ██║█████╗      ██║██╔██╗ ██║    ██║   ██║███████║██║██╔██╗ ██║
██║     ██║╚██╗ ██╔╝██╔══╝      ██║██║╚██╗██║    ╚██╗ ██╔╝██╔══██║██║██║╚██╗██║
███████╗██║ ╚████╔╝ ███████╗    ██║██║ ╚████║     ╚████╔╝ ██║  ██║██║██║ ╚████║
╚══════╝╚═╝  ╚═══╝  ╚══════╝    ╚═╝╚═╝  ╚═══╝      ╚═══╝  ╚═╝  ╚═╝╚═╝╚═╝  ╚═══╝
"""

from collections import defaultdict
from itertools import combinations

def yield_beacon_name():
    beacon_num = 0
    while True:
        yield 'beacon{}'.format(beacon_num)
        beacon_num += 1

def read_scanners():
    key = None
    scans = defaultdict(list)
    with open('puzzle19.dat') as f:
        for l in f.read().split('\n'):
            if l.startswith('---'):
                key = l[4:-4]
            elif l:
                coord = tuple(map(int, l.split(',')))
                scans[key].append(coord)
    return scans

def xfer_scanner_dists(beacon_scanner_dist, beacon_a, beacon_b):
    add_items = list()
    remove_items = list()
    for beacon_name, scanner_name in beacon_scanner_dist.keys():
        if beacon_name == beacon_b:
            add_items.append((beacon_a, scanner_name, beacon_scanner_dist[(beacon_name, scanner_name)]))
            remove_items.append((beacon_b, scanner_name))
    for item in add_items:
        beacon_scanner_dist[(item[0], item[1])] = item[2]
    for item in remove_items:
        del beacon_scanner_dist[(item[0], item[1])]

scans = read_scanners()
beacon_dists = defaultdict(set)
number_of_required_matches = 4
beacon_name_gen = yield_beacon_name()

beacon_scanner_dist = dict()

for scanner_name, scan in scans.items():
    coord_to_beacon = defaultdict(lambda: next(beacon_name_gen))
    for a, b in combinations(scan, 2):
        dist = (a[0] - b[0]) ** 2 + (a[1] - b[1]) ** 2 + (a[2] - b[2]) ** 2
        beacon_a_name = coord_to_beacon[a]
        beacon_b_name = coord_to_beacon[b]
        beacon_dists[beacon_a_name].add(dist)
        beacon_dists[beacon_b_name].add(dist)
        beacon_scanner_dist[(beacon_a_name, scanner_name)] = a
        beacon_scanner_dist[(beacon_b_name, scanner_name)] = b

while True:
    found_same = False
    for beacon_a, beacon_b in combinations(beacon_dists.keys(), 2):
        if beacon_a not in beacon_dists or beacon_b not in beacon_dists:
            continue
        inter = beacon_dists[beacon_a].intersection(beacon_dists[beacon_b])
        if len(inter) >= number_of_required_matches:
            found_same = True
            beacon_dists[beacon_a].update(beacon_dists[beacon_b].difference(inter))
            xfer_scanner_dists(beacon_scanner_dist, beacon_a, beacon_b)
            beacon_dists.pop(beacon_b)
    if not found_same:
        break

def manhattan(scanner_a, scanner_b):
    global scanner_position
    scanner_a_pos = scanner_position[scanner_a]
    scanner_b_pos = scanner_position[scanner_b]
    return abs(scanner_a_pos[0] - scanner_b_pos[0]) + abs(scanner_a_pos[1] - scanner_b_pos[1]) + abs(scanner_a_pos[2] - scanner_b_pos[2])

print (f"Part 1: {len(beacon_dists)} beacons of light in the murky depths")

first_scanner_name = next(k for k in scans.keys())
print (f"First scanner: {first_scanner_name}")
scanner_position = {first_scanner_name: (0,0,0)}

first_beacon = next(k for k in beacon_dists.keys())
beacon_position = dict()

print (f"First beacon: {first_beacon}")

while len(scanner_position) < len(scans):
    for scanner in scans:
        if scanner in scanner_position:
            scan_pos = scanner_position[scanner]
            for beacon in beacon_dists:
                if (beacon not in beacon_position) and ((beacon, scanner) in beacon_scanner_dist):
                    dist = beacon_scanner_dist[(beacon, scanner)]
                    beacon_position[beacon] = (scan_pos[0] + dist[0], scan_pos[1] + dist[1], scan_pos[2] + dist[2])
                    #print (f"Beacon {beacon} at {beacon_position[beacon]}")
                    break
        else:
            for beacon in beacon_dists:
                if beacon in beacon_position and (beacon, scanner) in beacon_scanner_dist:
                    dist = beacon_scanner_dist[(beacon, scanner)]
                    scanner_position[scanner] = (beacon_position[beacon][0] - dist[0], beacon_position[beacon][1] - dist[1], beacon_position[beacon][2] - dist[2])
                    #print (f"Scanner {scanner} at {scanner_position[scanner]}")
                    break

print ('Part 2:', max(manhattan(a,b) for a,b in combinations(scanner_position.keys(), 2)))

# 9275 too low
# 12000 too low
