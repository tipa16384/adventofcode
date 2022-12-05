from collections import defaultdict
import re
import heapq
from difflib import SequenceMatcher

with open("puzzle19.txt", "r") as f:
    input_str = f.read().splitlines()

# the last line is the molecule
molecule = input_str[-1]

# remove the last two lines
input_str = input_str[:-2]

# create a dictionary of replacements
replacements = defaultdict(list)

for line in input_str:
    # split the line into the two parts
    parts = line.split(" => ")

    # add the replacement to the dictionary
    replacements[parts[0]].append(parts[1])

def find_next_steps(replacements, molecule: str) -> set:
    # initialize the number of distinct molecules to 0
    distinct_molecules_set = set()

    # for each key in the dictionary
    for key in replacements:
        # for each replacement for the key
        for replacement in replacements[key]:
            # use regex to find all instances of the key
            for match in re.finditer(key, molecule):
                # create the new molecule
                new_molecule = molecule[:match.start()] + replacement + molecule[match.end():]

                # add the new molecule to the set
                distinct_molecules_set.add(new_molecule)
    
    return distinct_molecules_set

def reverse_find_next_steps(replacements, molecule: str) -> set:
    # initialize the number of distinct molecules to 0
    distinct_molecules_set = set()

    # for each key in the dictionary
    for key in replacements:
        # for each replacement for the key
        for replacement in replacements[key]:
            # use regex to find all instances of the key
            match_list = list(re.finditer(replacement, molecule))[::-1]
            for match in match_list:
                # create the new molecule
                new_molecule = molecule[:match.start()] + key + molecule[match.end():]

                # add the new molecule to the set
                distinct_molecules_set.add(new_molecule)
    
    return distinct_molecules_set

def find_shortest_path(replacements, starter_molecule, molecule):

    heap = []
    heapq.heappush(heap, (len(molecule), molecule, 0))

    while heap:
        _, current_molecule, steps = heapq.heappop(heap)
        #print (current_molecule, steps)

        if current_molecule == starter_molecule:
            return steps
        
        current_len = len(current_molecule)

        for next_molecule in reverse_find_next_steps(replacements, current_molecule):
            if len(next_molecule) >= current_len:
                continue
            heapq.heappush(heap, (len(next_molecule), next_molecule, steps + 1))

print ("Part 1:", len(find_next_steps(replacements, molecule)))
print ("Part 2:", find_shortest_path(replacements, "e", molecule))

