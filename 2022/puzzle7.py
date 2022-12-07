from day7file import File

def makeFileSystem(puzzle):
    root = File("/", 0, None, True)
    wd = root

    for l in puzzle:
        # if l starts with '$ cd ', change working directory
        if l.startswith("$ cd "):
            wd = wd.change_working_directory(l[5:])
        elif l.startswith("$ ls"):
            pass
        else:
            wd.add_file(l)
    return root

with open("2022\\puzzle7.txt") as f:
    root = makeFileSystem(f.read().splitlines())

# starting at root, make a list of all folders which have a calcSize of at most 100000
delete_me = root.find_folders(100000, lambda a, b: a <= b)
print ("Part 1: {}".format(sum([f.calcSize() for f in delete_me])))

space_needed = root.calcSize() - 40000000

# find smallest folder with size >= space_needed
delete_me = root.find_folders(space_needed, lambda a, b: a >= b)
print ("Part 2: {}".format(min([f.calcSize() for f in delete_me])))
