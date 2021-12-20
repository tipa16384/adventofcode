def build_index(image: list, x: int, y: int, step: int) -> int:
    index = 0
    for dy in range(y-1, y+2):
        for dx in range(x-1, x+2):
            if dx < 0 or dx >= len(image[0]) or dy < 0 or dy >= len(image):
                index = index << 1 | infinity(step)
            else:
                index = index << 1 | (1 if image[dy][dx] == '#' else 0)
    return index

def count_pixels(image) -> int: return sum(row.count('#') for row in image)

def process_image(image, key, num_steps) -> list:
    for step in range(num_steps):
        new_image = []
        for y in range(-1, len(image) + 1):
            row = ''.join(key[build_index(image, x, y, step)]
                        for x in range(-1, len(image[0]) + 1))
            new_image.append(row)
        image = new_image
    return image

with open('puzzle20.dat') as f:
    data = f.read().split('\n')
    key = data[0]
    image = data[2:]

if key[0] == '.':
    def infinity(_) -> int: return 0
else:
    def infinity(step: int) -> int: return 1 if step % 2 else 0

print("Part 1:", count_pixels(process_image(image, key, 2)))
print("Part 2:", count_pixels(process_image(image, key, 50)))
