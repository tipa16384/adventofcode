from Box import Box

b1 = Box(10,10,10,10,10,10)

print (b1)
assert b1.area() == 1

b2 = Box(10,10,10,12,12,12)
print (f"b2: {b2}")

assert b2.area() == 27

b3 = Box(11,11,11,13,13,13)
print (f"b3: {b3}")

print (b3&b2)

assert (b3&b2).area() == 8

print ("Intersect works!")

print(list(b3-b2))

for b in list(b3-b2):
    print(b.area(), b)

assert sum(b.area() for b in (b3-b2)) == 19

print("Difference works!")

b4 = Box(0,0,0,10,10,10)
b5 = Box(30,30,30,40,40,40)
print (b4, b5)
print (next(b4-b5))
assert next(b4-b5) == b4
assert next(b5-b4) == b5

# on x=-20..26,y=-36..17,z=-47..7
# on x=-20..33,y=-21..23,z=-26..28
# on x=-22..28,y=-29..23,z=-38..16

b0 = Box(-20,-36,-47,26,17,7)
b1 = Box(-20,-21,-26,33,23,28)

print (f"b0: {b0} - b1: {b1}")
expected_sum = b0.area() + b1.area() - (b0&b1).area()
print (f"expected_sum: {expected_sum}")
print (f"calculated sum: {sum(b.area() for b in list(b0-b1)) + b1.area()}")

b0 = Box(0,0,0,10,10,10)
b1 = Box(0,0,1,10,10,10)

print (f"b0: {b0} & b1: {b1} = {b0&b1}")
print (f"b0: {b0} - b1: {b1} = {list(b0-b1)}")
