class Container:
    def __init__(self, capacity):
        self.capacity = int(capacity)
    
    def __lt__(self, other):
        return self.capacity < other.capacity
    
def read_data():
    with open('puzzle17.dat') as f:
        return sorted(list(map(Container, f.read().split('\n'))), reverse=True)

def yield_solutions(used_containers, containers, target):
    total_capacity = sum(x.capacity for x in containers)
    if total_capacity == target:
        yield used_containers + containers
    elif total_capacity > target:
        for i, c in enumerate(containers):
            if c.capacity == target:
                yield used_containers + [c]
            elif c.capacity < target:
                yield from yield_solutions(used_containers + [c], containers[i+1:], target - c.capacity)

solutions = list(yield_solutions([], read_data(), 150))

print (f"Part 1: {len(solutions)}")

min_containers = min(len(solution) for solution in solutions)

print (f"Part 2: {len(list(solution for solution in solutions if len(solution) == min_containers))}")

