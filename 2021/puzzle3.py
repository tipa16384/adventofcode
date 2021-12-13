# this is going to be a super cool program
# that will be able to do a lot of cool stuff

fn = 'puzzle3.dat'

# number of bits in the binary string
width = 12


def read_values(fn):
    "read in fn as an array of binary numbers"
    with open(fn) as f:
        return [x.strip() for x in f.readlines()]


def binary_to_decimal(binary):
    "convert a binary string to an integer"
    return int(binary, 2)


def calc_gamma_epsilon(sieve):
    "calculate the gamma and epsilon values according to the rules of the puzzle."
    bitCount = [0 for x in range(width)]
    count = 0

    for x in sieve:
        count += 1
        for i in range(width):
            if x[i] == '1':
                bitCount[i] += 1

    gamma = epsilon = ''

    for i in bitCount:
        if i >= count/2:
            gamma += '1'
            epsilon += '0'
        else:
            gamma += '0'
            epsilon += '1'

    return (gamma, epsilon)


def process_sieve(sieve, func_select):
    """
    process the sieve according to the guidance of the function func_select
    return the solution or raise an exception if none found
    if none was found, you done screwed up
    """
    for pos in range(width):
        score = func_select(sieve)
        sieve = [x for x in sieve if x[pos] == score[pos]]
        if len(sieve) == 1:
            return sieve[0]
    raise Exception('No solution found')


# main function
if __name__ == '__main__':
    gamma, epsilon = calc_gamma_epsilon(read_values(fn))

    # part 1 solution
    print(binary_to_decimal(gamma) * binary_to_decimal(epsilon))

    oxygen = binary_to_decimal(process_sieve(
        read_values(fn), lambda x: calc_gamma_epsilon(x)[0]))
    co2 = binary_to_decimal(process_sieve(
        read_values(fn), lambda x: calc_gamma_epsilon(x)[1]))

    # part 2 solution
    print(oxygen * co2)
