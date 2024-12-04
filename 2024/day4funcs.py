def day4_data(file) -> int:
    return file.read().decode('utf-8').splitlines()

def word_search(data: list, word, grid_width, grid_height) -> int:
    offsets = [(0, 1), (1, 0), (0, -1), (-1, 0), (1, 1), (-1, -1), (1, -1), (-1, 1)]
    word_length = len(word)
    num_found = 0

    for row in range(grid_height):
        for col in range(grid_width):
            if data[row][col] == word[0]:
                for dr, dc in offsets:
                    end_row = row + dr * (word_length - 1)
                    end_col = col + dc * (word_length - 1)
                    if 0 <= end_row < grid_height and 0 <= end_col < grid_width:
                        if all(word[i] == data[row + dr * i][col + dc * i] for i in range(word_length)):
                            num_found += 1
    return num_found

def x_search(data: list, word, grid_width, grid_height) -> int:
    offsets = [(1, 1), (-1, 1), (1, -1), (-1, -1)]
    num_found = 0

    for row in range(1, grid_height - 1):
        for col in range(1, grid_width - 1):
            if data[row][col] == word[1]:
                if sum(data[row + dr][col + dc] == word[0] and data[row - dr][col - dc] == word[2] for dr, dc in offsets) == 2:
                    num_found += 1

    return num_found
