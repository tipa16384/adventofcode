number_segs = {'abcefg': '0', 'cf': '1',  'acdeg': '2', 'acdfg': '3', 'bcdf': '4',
               'abdfg': '5', 'abdefg': '6', 'acf': '7', 'abcdefg': '8', 'abcdfg': '9'}

def read_input():
    with open('puzzle8.dat') as f:
        for line in f:
            words = line.strip().split(' ')
            yield words[:10], words[11:15]

def figure_it_out(sample):
    decrypt_map = {}

    one = next(x for x in sample if len(x) == 2)
    seven = next(x for x in sample if len(x) == 3)
    four = next(x for x in sample if len(x) == 4)
    top = next(x for x in seven if x not in one)

    decrypt_map[top] = 'a'

    for word in sample:
        if len(word) == 5 and top in word and one[0] in word and one[1] in word:
            three = word

    for letter in four:
        if letter not in three:
            upper_left = letter
            decrypt_map[upper_left] = 'b'

    for word in sample:
        if len(word) == 5 and top in word and ((one[0] in word) ^ (one[1] in word)) and upper_left not in word:
            two = word
            upper_right = one[0] if one[0] in word else one[1]
            lower_right = one[1] if one[0] in word else one[0]
            break

    for word in sample:
        if len(word) == 5 and word != two:
            five = word

    for letter in two:
        if letter not in five and letter != upper_right:
            lower_left = letter
            decrypt_map[lower_left] = 'e'

    for letter in four:
        if letter not in [upper_right, upper_left, lower_right]:
            middle = letter
            decrypt_map[middle] = 'd'

    for letter in three:
        if letter not in [top, upper_right, middle, lower_right]:
            bottom = letter
            decrypt_map[bottom] = 'g'

    decrypt_map[upper_right] = 'c'
    decrypt_map[lower_right] = 'f'

    return decrypt_map

if __name__ == '__main__':
    score = sum(sum(1 for word in output if len(word) in [
                2, 3, 4, 7]) for _, output in read_input())

    print('Part 1:', score)

    total = 0

    for sample, output in read_input():
        decrypt_map = figure_it_out(sample)
        display = ''.join(number_segs[''.join(
            sorted(''.join([decrypt_map[letter] for letter in word])))] for word in output)
        total += int(display)

    print('Part 2:', total)
