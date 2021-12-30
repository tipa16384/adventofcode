import re
from timeit import default_timer as timer
import numpy as np

data = """Sprinkles: capacity 5, durability -1, flavor 0, texture 0, calories 5
PeanutButter: capacity -1, durability 3, flavor 0, texture 0, calories 1
Frosting: capacity 0, durability -1, flavor 4, texture 0, calories 6
Sugar: capacity -1, durability 0, flavor 0, texture 2, calories 8"""

class Ingredient:
    def __init__(self, line):
        self.name, self.capacity, self.durability, self.flavor, self.texture, self.calories = \
            re.match(
                r'(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)', line).groups()
        self.qualities = np.array([int(self.capacity), int(self.durability), int(
            self.flavor), int(self.texture), int(self.calories)])

def get_amounts(code, num_ingredients):
    amounts = []
    for _ in range(num_ingredients-1):
        amounts.append(code % radix + 1)
        code //= radix
    amounts.append(100 - sum(amounts))
    return amounts

def yield_scores(ingredients, calorie_counter=False):
    for i in range(radix**(len(ingredients)-1)):
        amounts = get_amounts(i, len(ingredients))
        if not any(amount <= 0 for amount in amounts):
            qualities = np.array(
                [amount * ingredient.qualities for amount, ingredient in zip(amounts, ingredients)]).sum(axis=0)
            if not any(qualities <= 0):
                if not calorie_counter or qualities[-1] == 500:
                    yield np.prod(qualities[:4])

ingredients = list(map(Ingredient, data.split('\n')))
radix = 101 - len(ingredients)

start = timer()
print(f"Part 1: {max(yield_scores(ingredients))} in {timer() - start:.4f} seconds")
start = timer()
print(f"Part 2: {max(yield_scores(ingredients, True))} in {timer() - start:.4f} seconds")
