# data is a list of lines from puzzle4.txt

# Path: 2017\puzzle5.py
# Compare this snippet from 2017\puzzle1a.py:
# # Get the input string
# with open('puzzle1.txt', 'r') as f:

with open('puzzle4.txt', 'r') as f:
    input_str = f.read().splitlines()

def hasDuplicatedWords(s: str) -> bool:
    words = s.split()
    return len(words) != len(set(words))

def hasWordsWithSameLetters(s: str) -> bool:
    words = s.split()
    for i in range(len(words)):
        for j in range(i + 1, len(words)):
            if set(words[i]) == set(words[j]):
                return True
    return False

print ("Part 1: ", len([x for x in input_str if not hasDuplicatedWords(x)]))
print ("Part 2: ", len([x for x in input_str if not hasWordsWithSameLetters(x)]))

