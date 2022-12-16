from functools import lru_cache
from time import sleep, time_ns
import re
from collections import defaultdict
from itertools import combinations
from threading import Thread

class SensorThread(Thread):
    def __init__(self, sensor, sensor_data):
        super().__init__()
        self.sensor = sensor
        self.sensor_data = sensor_data
        self.result = None
        self.stop_me = False

    def run(self):
        self.result = self.find_gap_for_sensor()

    def find_gap_for_sensor(self):
        # print ("sensor", sensor)
        skipping = None
        for c in periphery(self.sensor):
            if self.stop_me: return None
            if skipping and is_inside(skipping, *c):
                continue
            sleep(0)
            inside_s = is_inside(self.sensor_data, *c)
            if not inside_s:
                return c[0] * 4000000 + c[1]
            skipping = [inside_s]
        return None


def manhattan_distance(x1, y1, x2, y2):
    return abs(x1 - x2) + abs(y1 - y2)


def read_input():
    sensor_data = []
    with open(r"2022\puzzle15.txt") as f:
        data = f.read().splitlines()
        for line in data:
            sx, sy, bx, by = map(int, re.findall(r"([\-\d]+)", line))
            sensor_data.append(
                (sx, sy, bx, by, manhattan_distance(sx, sy, bx, by)))

    return sensor_data


def query_sensors(row):
    scanned_positions = set()

    for sx, sy, bx, by, mdist in sensor_data:
        md = manhattan_distance(sx, sy, sx, row)

        if md <= mdist:
            bloc = (bx, by)
            for x in range(sx - (mdist - md), sx + (mdist - md) + 1):
                if (x, row) != bloc:
                    scanned_positions.add(x)

    return len(scanned_positions)

def is_in_bounds(z):
    return z >= 0 and z <= 4000000


def periphery(sensor):
    # return the list of points on the periphery of the sensor
    sx, sy, _, _, mdist = sensor
    for i in range(mdist+2):
        if is_in_bounds(sx-mdist-1+i):
            if is_in_bounds(sy-i):
                yield (sx-mdist-1+i, sy-i)
            if is_in_bounds(sy+i):
                yield (sx-mdist-1+i, sy+i)
        if is_in_bounds(sx+mdist+1-i):
            if is_in_bounds(sy-i):
                yield (sx+mdist+1-i, sy-i)
            if is_in_bounds(sy+i):
                yield (sx+mdist+1-i, sy+i)

def is_inside(sensor_data, x, y):
    for sensor in sensor_data:
        sx, sy, _, _, mdist = sensor
        if manhattan_distance(sx, sy, x, y) <= mdist:
            return sensor
    return None


def find_gaps():
    for s in sensor_data:
        print ("sensor", s)
        skipping = None
        for c in periphery(s):
            if skipping and is_inside(skipping, *c):
                continue
            inside_s = is_inside(sensor_data, *c)
            if not inside_s:
                return c[0] * 4000000 + c[1]
            skipping = [inside_s]

sensor_data = read_input()

start = time_ns()
part1 = query_sensors(2000000)
print(f"Part 1: {part1} in {(time_ns() - start)/1e6}ms")

start = time_ns()

threads = [SensorThread(sensor, sensor_data) for sensor in sensor_data]
for t in threads:
    t.start()

# wait for any thread to have a result
result = None
while not result:
    for t in threads:
        if t.result:
            result = t.result
    sleep(0.1)

print(f"Part 2: {result} in {(time_ns() - start)/1e9}s")

for t in threads:
    t.stop_me = True
