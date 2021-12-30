import re
from timeit import default_timer as timer

data = """Sprinkles: capacity 5, durability -1, flavor 0, texture 0, calories 5
PeanutButter: capacity -1, durability 3, flavor 0, texture 0, calories 1
Frosting: capacity 0, durability -1, flavor 4, texture 0, calories 6
Sugar: capacity -1, durability 0, flavor 0, texture 2, calories 8"""

class Ingredient:
    def __init__(self, line):
        self.name, self.capacity, self.durability, self.flavor, self.texture, self.calories = \
            re.match(r'(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)', line).groups()
        self.capacity = int(self.capacity)
        self.durability = int(self.durability)
        self.flavor = int(self.flavor)
        self.texture = int(self.texture)
        self.calories = int(self.calories)

def get_amounts(code, num_ingredients):
    amounts = []
    for _ in range(num_ingredients-1):
        amounts.append(code % 100)
        code //= 100
    amounts.append(100 - sum(amounts))
    return amounts

def yield_scores(ingredients, calorie_counter = False):
    for i in range(100**(len(ingredients)-1)):
        amounts = get_amounts(i, len(ingredients))
        if not any(amount <= 0 for amount in amounts):
            capacity, durability, flavor, texture, calories = 0, 0, 0, 0, 0
            for j in range(len(ingredients)):
                capacity += ingredients[j].capacity * amounts[j]
                durability += ingredients[j].durability * amounts[j]
                flavor += ingredients[j].flavor * amounts[j]
                texture += ingredients[j].texture * amounts[j]
                calories += ingredients[j].calories * amounts[j]
            if capacity > 0 and durability > 0 and flavor > 0 and texture > 0:
                if not calorie_counter or calories == 500:
                    yield capacity * durability * flavor * texture

ingredients = list(map(Ingredient, data.split('\n')))

start = timer()
print (f"Part 1: {max(yield_scores(ingredients))} in {timer() - start:.4f} seconds")
start = timer()
print (f"Part 2: {max(yield_scores(ingredients, True))} in {timer() - start:.4f} seconds")
