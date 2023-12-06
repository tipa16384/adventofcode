def part1(data, seeds, mappers):
    print ("Part 1:", min([translate(mappers, 'seed', 'location', seed) for seed in seeds]))

def part2(data, seeds, mappers):
    results = [expand(mappers, 'seed', 'location', seed_range) for seed_range in zip(seeds[0::2], seeds[1::2])]
    
    # flatten the list of lists into a list
    results = [item for sublist in results for item in sublist]

    # answer is the minimum value of the first element of each tuple
    print ("Part 2:", min(x[0] for x in results))

# expand takes a source, dest, and a range and returns a list of ranges
def expand(mappers, source, dest, seed_range) -> list:
    if source == dest:
        return [seed_range]
    mapping = mappers[source]
    expanded = []
    for dest_start, source_start, length in mapping[1]:
        if seed_range[0] < source_start and seed_range[0] + seed_range[1] > source_start and seed_range[0] + seed_range[1] <= source_start + length:
            left_range = (seed_range[0], source_start - seed_range[0])
            right_range = (dest_start, seed_range[1] - left_range[1])
            expanded = expanded + expand(mappers, mapping[0], dest, right_range) + expand(mappers, source, dest, left_range)
            break
        elif seed_range[0] >= source_start and seed_range[0] < source_start + length and seed_range[0] + seed_range[1] > source_start + length:
            left_range = (dest_start + seed_range[0] - source_start, source_start + length - seed_range[0])
            right_range = (source_start + length, seed_range[0] + seed_range[1] - source_start - length)
            expanded = expanded + expand(mappers, mapping[0], dest, left_range) + expand(mappers, source, dest, right_range)
            break
        elif seed_range[0] >= source_start and seed_range[0] + seed_range[1] <= source_start + length:
            expanded = expanded + expand(mappers, mapping[0], dest, (dest_start + seed_range[0] - source_start, seed_range[1]))
            break
        elif seed_range[0] < source_start and seed_range[0] + seed_range[1] > source_start + length:
            left_range = (seed_range[0], source_start - seed_range[0])
            right_range = (source_start + length, seed_range[0] + seed_range[1] - source_start - length)
            middle_range = (dest_start, length)
            expanded = expanded + expand(mappers, source, dest, right_range) + expand(mappers, mapping[0], dest, middle_range) + expand(mappers, source, dest, left_range)
            break

    if not expanded:
        expanded = expanded + expand(mappers, mapping[0], dest, seed_range)

    return expanded

# translate takes a source, dest, and value and returns the translated value
def translate(mappers, source, dest, value):
    if source == dest:
        return value
    mapping = mappers[source]
    for dest_start, source_start, length in mapping[1]:
        if source_start <= value < source_start + length:
            return translate(mappers, mapping[0], dest, dest_start + (value - source_start))
    return translate(mappers, mapping[0], dest, value)

# function to take a string and return a tuple of the source map, dest map, and translation
def makeMap(data):
    map_data = data.splitlines()
    sourceMap, destMap = map_data[0].split()[0].split('-')[0], map_data[0].split()[0].split('-')[2]
    translation = [list(map(int, x.split())) for x in map_data[1:]]
    return (sourceMap, destMap, translation)

# function to read the file and split the data at blank lines
def readMap():
    with open('puzzle5.dat') as f:
        return f.read().split('\n\n')

if __name__ == "__main__":
    data = readMap()
    seeds = [int(x) for x in data[0].split()[1:]]
    mappers = {makeMap(mapper)[0]: makeMap(mapper)[1:] for mapper in data[1:]}
    part1(data, seeds, mappers)
    part2(data, seeds, mappers)
