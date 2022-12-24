with open(r'2022\puzzle22.txt') as f:
    rawinput = f.read()

# Parsing the input data

boardstate,instrs = rawinput.split('\n\n')
square_length = round(((boardstate.count('.')+boardstate.count('#'))/6)**0.5)
boardstate = [[char for char in line] for line in boardstate.splitlines()]
validsquares = {(i//square_length,j//square_length) for i in range(len(boardstate)) for j in range(len(boardstate[i])) if boardstate[i][j] != ' '}
boardstate = {(x,y): [[boardstate[x*square_length+i][y*square_length+j] for j in range(square_length)] for i in range(square_length)] for x,y in validsquares}
instrs = instrs.splitlines()[0].replace('R',',R,').replace('L',',L,').split(',')

# fold cube and get cube neighbours

def crossproduct(v1,v2):
    # cross product of 2 vectors
    x1,y1,z1 = v1
    x2,y2,z2 = v2
    x = y1*z2-z1*y2
    y = z1*x2-x1*z2
    z = x1*y2-y1*x2
    return [x,y,z]

def rotate_3d(plane_axis,vector,num):
    # rotates the vector by 90 degrees * n ccw. Assume plane_axis is pointing into the plane.
    for _ in range(num):
        vector = crossproduct(vector,plane_axis)
    return vector

def fold_cube(validsquares,anchor):
    cubes = {anchor: [[0,0,-1],[0,1,0]]} #anchor square has centre [0,0,-1] (-z) with net_right pointing to [0,1,0] (+y)
    remaining = {square for square in validsquares if square != anchor}
    while remaining:
        newcubes = {}
        for remsquare in remaining:
            for square,[centre,net_right] in cubes.items():
                difference = [i-j for i,j in zip(remsquare,square)]
                if difference in [[0,1],[-1,0],[0,-1],[1,0]]:
                    turn_offset = [[0,1],[-1,0],[0,-1],[1,0]].index(difference) #turn_offsets from Right. 1 is Up, 2 is Left, 3 is Down.
                    new_centre = rotate_3d(centre,net_right,turn_offset)
                    new_net_right = rotate_3d(new_centre,centre,(2-turn_offset)%4)
                    newcubes[remsquare] = [new_centre,new_net_right]
        cubes.update(newcubes)
        remaining = {square for square in remaining if square not in cubes}
    return cubes

def get_cube_neighbours(cubes): #gets neighbours of the cube at -z with associated turn.
    inversecubes = {tuple(centre): [square,net_right] for square,[centre,net_right] in cubes.items()} #rearrange
    output = {}
    for turn_offset,direction in enumerate([[0,1],[-1,0],[0,-1],[1,0]]):
        plane_axis = tuple(direction+[0])
        square,net_right = inversecubes[plane_axis]
        hyp_right = rotate_3d(plane_axis,[0,0,1],(-turn_offset)%4)
        count = 0
        while hyp_right != net_right:
            count+=1
            hyp_right = rotate_3d(plane_axis,hyp_right,1)
        key = 'RULD'[turn_offset]
        rotate_offset = 'NRTL'[count]
        output[key] = [square,rotate_offset]
    return output

square_links = {anchor: get_cube_neighbours(fold_cube(validsquares,anchor)) for anchor in validsquares}

# Walking along the board, using square_links to determine where I end up off the board.

def turn_direction(direction,turn): #direction can be R U L D. turn is L R N T.
    leftturnorder = 'RULD'
    numleftturns = 'NLTR'.index(turn)
    newindex = (leftturnorder.index(direction)+numleftturns)%4
    return leftturnorder[newindex]

def rotateboard(coords,square_length,turn):
    # outputs the end coords given the intial coords and a turn to rotate - N L T or R
    x,y = coords
    numleftturns = 'NLTR'.index(turn)
    for _ in range(numleftturns):
        x,y = square_length-1-y,x
    return (x,y)

class Board:
    def __init__(self,square_links,square_length,boardstate):
        self.square_links = square_links
        self.square_length = square_length
        self.boardstate = boardstate
        self.direction = 'R'
        self.board = min(square_links.keys())
        self.coords = (0,boardstate[self.board][0].index('.'))
    def move(self):
        direction = {'R':(0,1),'U':(-1,0),'L':(0,-1),'D':(1,0)}[self.direction]
        try_x,try_y = [i+j for i,j in zip(self.coords,direction)]
        if try_x in range(self.square_length) and try_y in range(self.square_length):
            if self.boardstate[self.board][try_x][try_y] == '.':
                self.coords = (try_x,try_y)
                return True
            else:
                return False
        else:
            try_board,turn = self.square_links[self.board][self.direction]
            try_x,try_y = (num%(self.square_length) for num in (try_x,try_y))
            try_x,try_y = rotateboard((try_x,try_y),self.square_length,turn)
            try_direction = turn_direction(self.direction,turn)
            if self.boardstate[try_board][try_x][try_y] == '.':
                self.coords = (try_x,try_y)
                self.board = try_board
                self.direction = try_direction
                return True
            else:
                return False
    def rotate(self,turn):
        self.direction = turn_direction(self.direction,turn)
            
board = Board(square_links,square_length,boardstate)

for instr in instrs:
    if instr in 'LR':
        board.rotate(instr)
    else:
        instr = int(instr)
        for _ in range(instr):
            moved = board.move()
            if not moved:
                break

board_x,board_y = board.board
x,y = board.coords
x = board_x*board.square_length+x+1
y = board_y*board.square_length+y+1
direction = ['R','D','L','U'].index(board.direction)
ans = 1000*x+4*y+direction
print(ans)
