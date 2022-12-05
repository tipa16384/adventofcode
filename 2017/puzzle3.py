input = 361527

def distance_to_input(n: int) -> int:
    # Compute the square root of the input number
    root = n ** 0.5

    # Get the whole part of the square root
    length = int(root)

    # If the whole part of the square root is odd, add 1 to the whole part
    # The nearest odd square is the square of this number
    if length % 2 == 0:
        length -= 1

    # Return the difference between the input value and the nearest odd square
    return length, n - (length ** 2)

def distance_to_center(n: int) -> int:
    # Compute the distance to the input value
    length, dist = distance_to_input(n)
    print (n, length, dist)

    # Get the distance to the center of the nearest odd square
    dist_to_center = length // 2
    print (n, length, dist, dist_to_center)

    x = dist_to_center
    y = -dist_to_center
    print ("Test 0", x, y, dist)

    if dist:
        x += 1
        dist -= 1
        print ("Test 1", x, y, dist)
    
    if dist >= length:
        y += length
        dist -= length
        print ("Test 2", x, y, dist)
    else:
        y += dist
        dist = 0
        print ("Test 3", x, y, dist)
    
    if dist >= length+1:
        x -= length+1
        dist -= length+1
        print ("Test 4", x, y, dist)
    else:
        x -= dist
        dist = 0
        print ("Test 5", x, y, dist)
    
    if dist >= length+1:
        y -= length+1
        dist -= length+1
        print ("Test 6", x, y, dist)
    else:
        y -= dist
        dist = 0
        print ("Test 7", x, y, dist)
    
    if dist >= length+1:
        x += length+1
        dist -= length+1
        print ("Test 8", x, y, dist)
    else:
        x += dist
        dist = 0
        print ("Test 9", x, y, dist)
    
    return abs(x) + abs(y)

print (distance_to_center(1))
print (distance_to_center(12))
print (distance_to_center(23))
print (distance_to_center(1024))
print (distance_to_center(input))

