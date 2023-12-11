def read_data():
    with open("puzzle9.dat") as f:
        return f.read().splitlines()
    
def calc_puzzle_right(val_list):
    # handle case of empty list or all zeros
    if not val_list or all(v == 0 for v in val_list): return val_list + [0]
    new_list = [(val_list[i+1] - val_list[i]) for i in range(len(val_list)-1)]
    return val_list + [val_list[-1] + calc_puzzle_right(new_list)[-1]]

print ("Part 1:", sum(calc_puzzle_right([int(x) \
                                for x in reading.split()])[-1] \
                                    for reading in read_data()))

print ("Part 2:", sum(calc_puzzle_right([int(x) \
                                for x in reading.split()[::-1]])[-1] \
                                    for reading in read_data()))
