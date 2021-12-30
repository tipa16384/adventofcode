import re
from timeit import default_timer as timer

class Reindeer:
    def __init__(self, line):
        self.name, self.speed, self.fly_time, self.rest_time = re.match(
            r'(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.', line).groups()
        self.speed = int(self.speed)
        self.fly_time = int(self.fly_time)
        self.cycle_time = int(self.rest_time) + self.fly_time
        self.score = 0

    def distance_at_time(self, time):
        return (time // self.cycle_time * self.fly_time +
                min(time % self.cycle_time, self.fly_time)) * self.speed

    def bump_score(self, time, lead_distance):
        if self.distance_at_time(time) == lead_distance:
            self.score += 1

with open('reindeer14.dat') as f:
    reindeer = [Reindeer(line) for line in f.read().split('\n')]

target = 2503

start = timer()
print(f"Part 1: {max(r.distance_at_time(target) for r in reindeer)} in {timer() - start:.3f} seconds")

start = timer()
for time in range(1, target + 1):
    for r in reindeer:
        r.bump_score(time, max(r.distance_at_time(time) for r in reindeer))

print(f"Part 2: {max(r.score for r in reindeer)} in {timer() - start:.3f} seconds")
