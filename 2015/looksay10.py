from itertools import groupby

puzzle = "1321131112"

def look_say(molecule: str, step:int) -> int:
    if step == 0:
        return len(molecule)
    new_molecule = ''.join(str(len(list(g))) + k for k, g in groupby(molecule))
    return look_say(new_molecule, step-1)

print ("Part 1:", look_say(puzzle, 40))
print ("Part 2:", look_say(puzzle, 50))

