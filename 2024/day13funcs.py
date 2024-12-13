import re
import numpy as np

def day13data(file):
    pattern = re.compile(r'(\d+), Y(?:\=|\+)(\d+)')
    systems = file.read().decode('utf-8').split('\n\n')
    equations = list()
    for system in systems:
        ab_list = list()
        for line in system.splitlines():
            ab_list.append(list(map(int, pattern.search(line).groups())))
        equations.append(ab_list)
    return equations

def parts(equations, correction = 0):
    tokens = 0

    for equation in equations:
        a = np.array([[equation[0][0], equation[1][0]], [equation[0][1], equation[1][1]]])
        b = np.array([equation[2][0] + correction, equation[2][1] + correction])
        solution = np.linalg.solve(a, b)
        x, y = map(round, solution)
        a = equation[0][0] * x + equation[1][0] * y
        b = equation[0][1] * x + equation[1][1] * y
        if x >= 0 and y >= 0 and \
            a == equation[2][0] + correction and b == equation[2][1] + correction:
            tokens += x*3 + y

    return tokens
