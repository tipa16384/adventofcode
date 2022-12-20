import re
from functools import lru_cache

class Blueprint:
    def __init__(self, deets):
        deeters = map(int, re.findall(r"\d+", deets))
        
        self.number = next(deeters)
        self.ore_robot_ore_cost = next(deeters)
        self.clay_robot_ore_cost = next(deeters)
        self.obsidian_robot_ore_cost = next(deeters)
        self.obsidian_robot_clay_cost = next(deeters)
        self.geode_robot_ore_cost = next(deeters)
        self.geode_robot_obsidian_cost = next(deeters)

    def __repr__(self):
        return f"Blueprint({self.number}, {self.ore_robot_ore_cost}, {self.clay_robot_ore_cost}, {self.obsidian_robot_ore_cost}, {self.obsidian_robot_clay_cost}, {self.geode_robot_ore_cost}, {self.geode_robot_obsidian_cost})"

class GameState:
    def __init__(self, from_tuple=None):
        self.ore = 0
        self.clay = 0
        self.obsidian = 0
        self.geode = 0
        self.ore_robots = 1
        self.clay_robots = 0
        self.obsidian_robots = 0
        self.geode_robots = 0
        self.name = "initial state"

        if from_tuple:
            self.ore, self.clay, self.obsidian, self.geode, self.ore_robots, self.clay_robots, self.obsidian_robots, self.geode_robots = from_tuple
    
    def __repr__(self):
        return f"GameState({self.ore}, {self.clay}, {self.obsidian}, {self.geode}, {self.ore_robots}, {self.clay_robots}, {self.obsidian_robots}, {self.geode_robots})"

    def __str__(self):
        return f"GameState({self.name})"

    def to_tuple(self):
        return (self.ore, self.clay, self.obsidian, self.geode, self.ore_robots, self.clay_robots, self.obsidian_robots, self.geode_robots)

    def copy(self):
        new_state = GameState()
        new_state.ore = self.ore
        new_state.clay = self.clay
        new_state.obsidian = self.obsidian
        new_state.geode = self.geode
        new_state.ore_robots = self.ore_robots
        new_state.clay_robots = self.clay_robots
        new_state.obsidian_robots = self.obsidian_robots
        new_state.geode_robots = self.geode_robots
        new_state.name = self.name
        return new_state

def read_blueprints():
    with open(r"2022\puzzle19.txt") as f:
        return [Blueprint(deets) for deets in f]

def produce(game_state_tuple):
    new_state = GameState(game_state_tuple)
    new_state.ore += game_state_tuple[4]
    new_state.clay += game_state_tuple[5]
    new_state.obsidian += game_state_tuple[6]
    new_state.geode += game_state_tuple[7]
    return new_state

@lru_cache(maxsize=None)
def get_new_states(game_state_tuple, blueprint_number):
    game_state = GameState(game_state_tuple)
    blueprint = blueprints[blueprint_number-1]
    max_ore = max(blueprint.ore_robot_ore_cost, blueprint.clay_robot_ore_cost, blueprint.obsidian_robot_ore_cost, blueprint.geode_robot_ore_cost)
    new_tuples = []


    if game_state.ore >= blueprint.geode_robot_ore_cost and game_state.obsidian >= blueprint.geode_robot_obsidian_cost:
        new_state = produce(game_state_tuple)
        new_state.ore -= blueprint.geode_robot_ore_cost
        new_state.obsidian -= blueprint.geode_robot_obsidian_cost
        new_state.geode_robots += 1
        new_state.name = "produced geode robot"
        return [new_state.to_tuple()]

    if game_state.obsidian_robots < blueprint.geode_robot_obsidian_cost and game_state.ore >= blueprint.obsidian_robot_ore_cost and game_state.clay >= blueprint.obsidian_robot_clay_cost:
        new_state = produce(game_state_tuple)
        new_state.ore -= blueprint.obsidian_robot_ore_cost
        new_state.clay -= blueprint.obsidian_robot_clay_cost
        new_state.obsidian_robots += 1
        new_state.name = "produced obsidian robot"
        new_tuples.append(new_state.to_tuple())


    if game_state.clay_robots < blueprint.obsidian_robot_clay_cost and game_state.ore >= blueprint.clay_robot_ore_cost:
        new_state = produce(game_state_tuple)
        new_state.ore -= blueprint.clay_robot_ore_cost
        new_state.clay_robots += 1
        new_state.name = "produced clay robot"
        new_tuples.append(new_state.to_tuple())

    if game_state.ore_robots < max_ore and game_state.ore >= blueprint.ore_robot_ore_cost:
        new_state = produce(game_state_tuple)
        new_state.ore -= blueprint.ore_robot_ore_cost
        new_state.ore_robots += 1
        new_state.name = "produced ore robot"
        new_tuples.append(new_state.to_tuple())
        
    new_state = produce(game_state_tuple)
    new_state.name = "produced nothing"
    new_tuples.append(new_state.to_tuple())

    return new_tuples

@lru_cache(maxsize=None)
def done_yet(game_state_tuple, blueprint_number, minute=0, max_minute=24):
    if minute >= max_minute:
        #print (minute, game_state, game_state.geode)
        return game_state_tuple[3]
    return max(done_yet(new_state, blueprint_number, minute+1, max_minute) for new_state in get_new_states(game_state_tuple, blueprint_number))

def solve(solve_blueprints, max_minute):
    initial_game_state = GameState().to_tuple()
    quality_level = 0
    for blueprint in solve_blueprints:
        done_yet.cache_clear()
        get_new_states.cache_clear()
        geode = done_yet(initial_game_state, blueprint.number, 0, max_minute)
        print (geode, geode * blueprint.number)
        quality_level += geode * blueprint.number
    return quality_level

blueprints = read_blueprints()

print ("Part 1:", solve(blueprints, 24))
print ("Part 2:", solve(blueprints[:3], 32))

