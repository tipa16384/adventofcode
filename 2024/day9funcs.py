def day9data(file) -> list:
    input_data = file.read().decode('utf-8')
    file_system = []
    file_id = 0
    is_file = True

    for block in map(int, input_data):
        file_system.extend([file_id if is_file else -1] * block)
        if is_file:
            file_id += 1
        is_file = not is_file

    return file_system

def day9part1(file_system_pristine: list) -> int:
    file_system = file_system_pristine.copy()
    checksum = 0
    start = 0
    end = len(file_system) - 1
    while start <= end:
        block_id = file_system[start]
        if block_id == -1:
            if file_system[end] == -1:
                end -= 1
                continue
            block_id = file_system[end]
            end -= 1
        checksum += block_id * start
        start += 1
    return checksum

def day9part2(file_system: list) -> int:
    run_file_system = file_system.copy()

    for run in run_generator(run_file_system):
        for free in free_generator(file_system):
            if run[1] < free[1]: break
            if run[2] > free[2]: continue
            free_start = free[1]
            run_start = run[1]
            for i in range(run[2]):
                file_system[free_start+i] = run[0]
                file_system[run_start+i] = -1
            break

    checksum = sum(i * val for i, val in enumerate(file_system) if val > -1)

    return checksum

def run_generator(file_system: list):
    current_run = None
    count = 0
    for i in range(len(file_system) - 1, -1, -1):
        token = file_system[i]
        if token == current_run:
            count += 1
        else:
            if current_run is not None:
                yield (current_run, i + 1, count)
            current_run = token
            count = 1
    if current_run is not None:
        yield (current_run, 0, count)

def free_generator(file_system: list):
    current_start = None
    count = 0
    for i, token in enumerate(file_system):
        if token == -1:
            if current_start is None:
                current_start = i
            count += 1
        else:
            if current_start is not None:
                yield (-1, current_start, count)
                current_start = None
                count = 0
    if current_start is not None:
        yield (-1, current_start, count)
