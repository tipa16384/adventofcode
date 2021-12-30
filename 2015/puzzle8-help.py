from os import path
import sys
import re

lines = list()

with open("escape8.dat") as f:
  for line in f:
    line = line.strip()
    lines.append(line)

def partOne(lines: list) -> int:
  totalCharacters, totalCharactersInMemory = 0, 0
  for line in lines:
    #line = repr(line[1:-1])
    pattern = re.compile(r'\\x\w\w|\\"|\\\\')
    matches = pattern.findall(line)
    hexChar, quote, backslash = r"\\x\w\w", r'\\"', r"\\\\"
    hexChars, quotes, backslashes = re.findall(hexChar, line), re.findall(quote, line), re.findall(backslash, line)
    lineLen = len(line) - int(len(backslashes) // 2)
    charsInMem = lineLen - 2 # - len(hexChars) - len(quotes) - len(backslashes)
    for c in hexChars:
      charsInMem -= len(c)
      charsInMem += 1
    for c in quotes:
      charsInMem -= len(c)
      charsInMem += 1
    print(line)

    # print("HexChars: " + str(hexChars))
    # print("Quotes: " + str(quotes))
    # print("Backslashes: " + str(backslashes))
    # print("LineLen: " + str(lineLen))
    # print("CharsInMem: " + str(charsInMem))
    # print()

    totalCharacters += lineLen
    totalCharactersInMemory += charsInMem
  return totalCharacters - totalCharactersInMemory

print("PartOne: " + str(partOne(lines)))
