from datetime import datetime
from jinja2 import Template

# Define the dictionary with day titles
days = {
    1: "Historian Hysteria",
    2: "Red-Nosed Reports",
    3: "Mull It Over",
    4: "Ceres Search",
    5: "Print Queue",
    6: "Guard Gallivant",
    7: "Bridge Repair",
    8: "Resonant Collinearity",
    9: "Disk Fragmenter",
    10: "Hoof It",
    11: "Plutonian Pebbles",
    12: "Garden Groups",
    13: "Claw Contraption",
    14: "Restroom Redoubt",
    15: "Warehouse Woes",
    16: "Reindeer Maze",
    17: "Chronospatial Computer",
    18: "RAM Run",
    19: "Linen Layout",
    20: "Race Condition",
    21: "Keypad Conundrum",
    22: "Monkey Market"
}

def generate_index():
    print ("Generating index...")
    # Get the current day
    current_day = datetime.now().day

    # Read the HTML file
    with open("2024/aoc2024.html", "r") as file:
        html_template = file.read()

    # Create the Jinja2 template
    template = Template(html_template)

    # Render the HTML
    rendered_html = template.render(
        days=days,
        current_day=current_day
    )

    # delete the old file
    import os
    try:
        os.remove("2024/aoc2024_rendered.html")
    except:
        pass
    # Write the output to a new HTML file
    with open("2024/aoc2024_rendered.html", "w") as file:
        file.write(rendered_html)
