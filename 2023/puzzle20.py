from enum import Enum
from queue import Queue
from math import lcm

button_presses = 0

# enum with two states, HIGH and LOW
State = Enum('State', ['HIGH', 'LOW'])

# pulse_list is a FIFO queue that holds (Component dest, Component source, State) tuples
pulse_list = Queue()

# class Component with two functions, pulse_in, that takes a State as parameter, and pulse_out, that
# calls the pulse_in function for the each Component in the list of Component in the Component class.
# the base class's implementation takes the input from pulse_in and sends it unchanged to pulse_out.
class Broadcast:
    def __init__(self, name):
        self.name = name
        self.outputs = []
        self.inputs = []
        self.am_feeder = False
    
    def pulse_in(self, source, state: State, pulse_list: Queue):
        self.pulse_out(state, pulse_list)
        
    def pulse_out(self, state: State, pulse_list: Queue):
        for output in self.outputs:
            pulse_list.put((output, self, state))
    
    def add_output(self, output):
        self.outputs.append(output)
        
    def add_input(self, input):
        self.inputs.append(input)

# declare Conjunction class subclass of Component
class Conjunction(Broadcast):
    def __init__(self, name):
        super().__init__(name)
        self.input_states = {}
        self.input_flipped = {}
        self.old_binary = ''

    def add_input(self, input):
        super().add_input(input)
        self.input_states[input] = State.LOW
        self.input_flipped[input] = 0

    def pulse_in(self, source, state: State, pulse_list: Queue):
        self.input_states[source] = state
        if state == State.HIGH and self.input_flipped[source] == 0:
            self.input_flipped[source] = button_presses
                    
        if all(state == State.HIGH for state in self.input_states.values()):
            send_state = State.LOW
        else:
            send_state = State.HIGH
        self.pulse_out(send_state, pulse_list)
    
    def all_flipped(self):
        return all(self.input_flipped.values())

# declare FlipFlop class subclass of Component
class FlipFlop(Broadcast):
    def __init__(self, name):
        super().__init__(name)
        self.state = State.LOW

    def pulse_in(self, source, state: State, pulse_list: Queue):
        if state == State.LOW:
            self.state = State.HIGH if self.state == State.LOW else State.LOW
            self.pulse_out(self.state, pulse_list)

class Button(Broadcast):
    def __init__(self, name='Button'):
        super().__init__(name)

    def pulse_in(self, source, state: State, pulse_list: Queue):
        self.pulse_out(State.LOW, pulse_list)

class Output(Broadcast):
    def __init__(self, name='output'):
        super().__init__(name)
        self.flipped = False
        
    def pulse_in(self, source, state: State, pulse_list: Queue):
        if state == State.LOW:
            print ("Flipped")
            self.flipped = True

def create_component(name, name_dict, component_dict):
    """Creates a component based on the given name and updates the dictionaries."""
    if name == 'broadcaster':
        component = Broadcast(name)
        clean_name = name
    elif name.startswith('&'):
        # Strip '&' from name for Conjunction components
        clean_name = name[1:]
        component = Conjunction(clean_name)
    else:
        # Assume FlipFlop for other components
        clean_name = name[1:]
        component = FlipFlop(clean_name)
        
    name_dict[name] = clean_name
    component_dict[clean_name] = component

def connect_components(ins, outs, name_dict, component_dict):
    """Connects components based on inputs and outputs."""
    for out in outs.split(', '):
        if out not in component_dict:
            component_dict[out] = Output(out)
        
        component = component_dict[name_dict[ins]]
        component.add_output(component_dict[out])
        component_dict[out].add_input(component)

def read_data():
    with open('puzzle20.dat') as f:
        data = f.read().splitlines()
    
    component_dict = {'button': Button()}
    name_dict = {}

    # First pass, create components
    for line in data:
        ins, outs = line.split(' -> ')
        create_component(ins, name_dict, component_dict)
        
    # Second pass, connect the components
    for line in data:
        ins, outs = line.split(' -> ')
        connect_components(ins, outs, name_dict, component_dict)
    
    # Connect button to broadcaster as a special case
    component_dict['button'].add_output(component_dict['broadcaster'])
    component_dict['broadcaster'].add_input(component_dict['button'])
    
    return component_dict

def push_the_button(component_dict: dict, counts: dict):
    component_dict['button'].pulse_in(None, State.LOW, pulse_list)

    while not pulse_list.empty():
        dest, source, state = pulse_list.get()
        counts[state] += 1
        dest.pulse_in(source, state, pulse_list)

def part1():
    component_dict = read_data()
    counts = {State.HIGH: 0, State.LOW: 0}

    for _ in range(1000):
        push_the_button(component_dict, counts)

    print ("Part 1:", counts[State.HIGH] * counts[State.LOW])

def part2():
    global button_presses
    component_dict = read_data()
    counts = {State.HIGH: 0, State.LOW: 0}

    # find the conjunction that feeds 'rx'
    feeder = component_dict['rx'].inputs[0]
    feeder.am_feeder = True

    while True:
        button_presses += 1
        push_the_button(component_dict, counts)
        if feeder.all_flipped():
            print ("Part 2:", lcm(*feeder.input_flipped.values()))
            break

part1()
part2()
