import re
from functools import reduce

puzzle_data_file = 'puzzle4.dat'


def get_balls(fp):
    "given a file pointer, return the first line as an array of comma-separated integers"
    return [int(x) for x in fp.readline().split(',')]


def get_board(fp):
    """
    given a file pointer, skip the first line and read the next five lines as a
    5x5 board of integers separated by spaces
    """
    while fp.readline() != '':
        board = []
        for i in range(5):
            board.append([int(x)
                          for x in re.split(u'\s+', fp.readline().strip())])
        yield board


def sanity_check():
    "open the puzzle data file and print the return from get_balls() and get_board()"
    with open(puzzle_data_file, 'r') as fp:
        print(get_balls(fp))
        for board in get_board(fp):
            print(board)


def mark_ball_in_board(ball, board):
    "mark the ball in the board"
    for row in board:
        if ball in row:
            row[row.index(ball)] = -1


def mark_ball_in_boards(ball, boards):
    "mark the ball in all the boards"
    for board in boards:
        mark_ball_in_board(ball, board)


def winning_board(board):
    "return True if any row or column is -1"
    row_winners = [sum(row) == -5 for row in board]
    col_winners = [sum([row[i] for row in board]) == -5 for i in range(5)]
    return reduce(lambda x, y: x or y, row_winners + col_winners)


def get_winning_boards(boards):
    "return a list of winning boards"
    return [board for board in boards if winning_board(board)]


def play_bingo(part2=False):
    "read the puzzle data file and get the balls and the board"
    with open(puzzle_data_file, 'r') as fp:
        balls = get_balls(fp)
        boards = list(get_board(fp))
        for ball in balls:
            mark_ball_in_boards(ball, boards)
            winning_boards = get_winning_boards(boards)
            if winning_boards:
                if not part2 or len(boards) == 1:
                    assert len(winning_boards) == 1
                    return ball, winning_boards[0]
                else:
                    for board in winning_boards:
                        boards.remove(board)

    raise Exception('no winning board found')


def score_board(ball, board):
    "return product of the ball and the sum of all the numbers in the board that are not -1"
    return ball * sum([sum([x for x in row if x != -1]) for row in board])


if __name__ == '__main__':
    # sanity_check()
    ball, board = play_bingo()
    print(score_board(ball, board))
    ball, board = play_bingo(True)
    print(score_board(ball, board))
