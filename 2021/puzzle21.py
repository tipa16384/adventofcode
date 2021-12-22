from functools import cache, reduce
from timeit import default_timer as timer
from itertools import cycle, islice, product
from tupleops import addtuple, multuple

def get_player_info(player: int, game_state: tuple) -> tuple:
    return (game_state[1], game_state[3]) if player else (game_state[0], game_state[2])

def put_player_info(player: int, game_state: tuple, new_pos: int, new_score: int) -> tuple:
    if player:
        return (game_state[0], new_pos, game_state[2], new_score)
    else:
        return (new_pos, game_state[1], new_score, game_state[3])

@cache
def fix_pos(pos: int, move: int) -> int:
    pos_cycle = islice(cycle(range(1, 11)), pos, None)
    for _ in range(move):
        pos = next(pos_cycle)
    return pos

def roll_die():
    die = cycle(range(1, 101))
    while True:
        yield sum(next(die) for _ in range(3))

def play_game1():
    start = timer()
    player_positions = [1, 3]
    player_scores = [0, 0]
    player = 0
    for roll_num, roll in enumerate(roll_die(), start=1):
        player_positions[player] = fix_pos(player_positions[player], roll)
        player_scores[player] += player_positions[player]
        if player_scores[player] >= 1000:
            print(f"Part 1: {roll_num * 3 * min(player_scores)} in {timer() - start:.3f} seconds")
            break
        player = 1 - player

#@cache
def play_game2(player: int, game_state: tuple) -> tuple:
    if get_player_info(player, game_state)[1] >= 21:
        return (0, 1) if player else (1, 0)

    player = 1 - player
    pos, score = get_player_info(player, game_state)

    return reduce(addtuple, (multuple(play_game2(player, put_player_info(
        player, game_state, fix_pos(pos, k), score + fix_pos(pos, k))), roll_map[k]) for k in roll_map))


possible_rolls = list(sum(x) for x in product(range(1, 4), repeat=3))
roll_map = dict(map(lambda x: (x, possible_rolls.count(x)), set(possible_rolls)))
print (roll_map)

play_game1()

start = timer()
wins = play_game2(1, (1, 3, 0, 0))
print(f"Part 2: {max(wins[0], wins[1])} in {timer() - start:.3f} seconds")
