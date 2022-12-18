from itertools import cycle

def read_hot_jets():
    with open(r"2022\puzzle17.txt") as f:
        jet_jet_jet = f.read()
        return len(jet_jet_jet), cycle(jet_jet_jet)

def get_pieces():
    pieces = []
    pieces.append([(0,0), (1,0), (2,0), (3,0)])
    pieces.append([(1,0), (0,1), (1,1), (2,1), (1,2)])
    pieces.append([(0,0), (1,0), (2,0), (2,1), (2,2)])
    pieces.append([(0,0), (0,1), (0,2), (0,3)])
    pieces.append([(0,0), (1,0), (0,1), (1,1)])
    return len(pieces), cycle(pieces)

def check_collision(playing_board, piece, piece_pos, delta):
    for x,y in piece:
        nx, ny = piece_pos[0]+x+delta[0], piece_pos[1]+y+delta[1]
        if (nx, ny) in playing_board:
            return True
        if nx < 0 or nx >= 7 or ny < 0:
            return True
    return False

def get_rock_height(playing_board):
    return max([y+1 for _,y in playing_board]) if playing_board else 0

def drop_a_rock(playing_board: set, pieces, jets, rest_at: list):
    piece = next(pieces)
    piece_pos = (2, get_rock_height(playing_board)+3)
    
    while True:
        jet = next(jets)
        #print(piece_pos, jet)
        dx = -1 if jet == '<' else 1
        if not check_collision(playing_board, piece, piece_pos, (dx, 0)):
            piece_pos = (piece_pos[0]+dx, piece_pos[1])
        if check_collision(playing_board, piece, piece_pos, (0, -1)):
            # add all piece points to the playing board
            for x,y in piece:
                playing_board.add((piece_pos[0]+x, piece_pos[1]+y))
            rest_at.append((*piece_pos, piece))
            #print ("dropped")
            break
        piece_pos = (piece_pos[0], piece_pos[1]-1)

def run_simulation(part1_drops):
    pieces_length, pieces = get_pieces()
    _, jets = read_hot_jets()
    playing_board = set()
    rest_at = list()

    for _ in range(part1_drops):
        drop_a_rock(playing_board, pieces, jets, rest_at)
    
    return rest_at, pieces_length

def get_pattern(rest_at, pieces_length, total_drops):
    confirmation_size = 4
    max_pattern_size = total_drops // confirmation_size // pieces_length
    print (max_pattern_size)

    pattern_found = False
    for i in range(max_pattern_size):
        pattern_length = (i+1)*pieces_length
        for test_start in range(0, total_drops - confirmation_size*pattern_length):
            pattern_found = True
            for j in range(test_start, test_start+pattern_length):
                if rest_at[j][0] != rest_at[j+pattern_length][0] or \
                    rest_at[j][0] != rest_at[j+2*pattern_length][0] or \
                    rest_at[j][0] != rest_at[j+3*pattern_length][0]:
                    pattern_found = False
                    break
            if pattern_found:
                return test_start, pattern_length
        if pattern_found:
            break

def calc_height_at(total_drops, rest_at, test_start, pattern_length, height_of_pattern):
    starting_height = rest_at[test_start][1]
    print ("--- Starting height", starting_height)
    num_pattern_repeats = (total_drops - test_start) // pattern_length
    print ("--- Num pattern repeats", num_pattern_repeats)
    leftover = (total_drops - test_start) % pattern_length
    print ("--- Leftover", leftover)
    leftover_height = rest_at[test_start+leftover][1] - rest_at[test_start][1]
    print ("--- Leftover height", leftover_height)
    return starting_height + num_pattern_repeats*height_of_pattern + leftover_height

ballpark_figure = 10000
rest_at, pieces_length = run_simulation(ballpark_figure)
test_start, pattern_length = get_pattern(rest_at, pieces_length, ballpark_figure)

print ("Pattern found at", test_start, "with length", pattern_length)
height_of_pattern = rest_at[test_start+pattern_length][1] - rest_at[test_start][1]
print ("Height of pattern", height_of_pattern)

print ("Part 1:", calc_height_at(2022, rest_at, test_start, pattern_length, height_of_pattern))
print ("Part 2:", calc_height_at(1000000000000, rest_at, test_start, pattern_length, height_of_pattern))
