from flask import Flask, jsonify, request
from flask_cors import CORS
import concurrent.futures

app = Flask(__name__)
CORS(app)

def build_response(part1, part2):
    return jsonify({'part1': part1, 'part2': part2})

def get_file_from_request():
    file = request.files.get('file')
    if not file or file.filename == '':
        error_message = 'No file part in the request' if not file else 'No selected file'
        return jsonify({'error': error_message}), False
    return file, True

@app.route('/2024/1', methods=['POST'])
def post_sum_diffs():
    from day1funcs import sum_diffs, sum_similar, day1_data
    file, success = get_file_from_request()
    if not success:
        return file, 400
    left, right = day1_data(file)
    return build_response(*run_threads((sum_diffs, left, right), (sum_similar, left, right)))

@app.route('/2024/2', methods=['POST'])
def day_2_rest():
    from day2funcs import day2_data, count_safe, count_kinda_safe
    file, success = get_file_from_request()
    if not success:
        return file, 400
    list_of_lists = day2_data(file)
    return build_response(*run_threads((count_safe, list_of_lists), (count_kinda_safe, list_of_lists)))

@app.route('/2024/3', methods=['POST'])
def day_3_rest():
    from day3funcs import day3_data, scan_and_multiply, scan_and_maybe_multiply
    file, success = get_file_from_request()
    if not success:
        return file, 400
    data = day3_data(file)
    return build_response(*run_threads((scan_and_multiply, data), (scan_and_maybe_multiply, data)))

@app.route('/2024/4', methods=['POST'])
def day_4_rest():
    from day4funcs import day4_data, word_search, x_search
    file, success = get_file_from_request()
    if not success:
        return file, 400
    data = day4_data(file)
    grid_width = len(data[0])
    grid_height = len(data)
    return build_response(*run_threads((word_search, data, 'XMAS', grid_width, grid_height),
                                 (x_search, data, 'MAS', grid_width, grid_height)))

@app.route('/2024/5', methods=['POST'])
def day_5_rest():
    from day5funcs import day5_data, beide_teile
    file, success = get_file_from_request()
    if not success:
        return file, 400
    return build_response(*beide_teile(*day5_data(file)))

@app.route('/2024/6', methods=['POST'])
def day_6_rest():
    from day6funcs import day6_data, find_path, find_obstructions
    file, success = get_file_from_request()
    if not success:
        return file, 400
    data = day6_data(file)
    part1, visited_cells = find_path(*data)
    part2 = find_obstructions(*data, visited_cells)
    return build_response(part1, part2)

@app.route('/2024/7', methods=['POST'])
def day_7_rest():
    from day7funcs import day7_data, parser
    file, success = get_file_from_request()
    if not success:
        return file, 400
    data = day7_data(file)
    part1 = parser(data, ['+', '*'])
    part2 = parser(data, ['+', '*', '||'])
    return build_response(part1, part2)

@app.route('/2024/8', methods=['POST'])
def day_8_rest():
    from day8funcs import day8_data, day8part1, day8part2
    file, success = get_file_from_request()
    if not success:
        return file, 400
    data, grid_height, grid_width = day8_data(file)
    part1 = day8part1(data, grid_height, grid_width)
    part2 = day8part2(data, grid_height, grid_width)
    return build_response(part1, part2)

@app.route('/2024/9', methods=['POST'])
def day_9_rest():
    from day9funcs import day9data, day9part1, day9part2
    file, success = get_file_from_request()
    if not success:
        return file, 400
    file_system = day9data(file)
    part1 = day9part1(file_system)
    part2 = day9part2(file_system)
    return build_response(part1, part2)

@app.route('/2024/10', methods=['POST'])
def day_10_rest():
    from day10funcs import day10data, day10parts
    file, success = get_file_from_request()
    if not success:
        return file, 400
    data = day10data(file)
    part1, part2 = day10parts(data)
    return build_response(part1, part2)

@app.route('/2024/11', methods=['POST'])
def day_11_rest():
    from day11funcs import day11data, day11parts
    file, success = get_file_from_request()
    if not success:
        return file, 400
    data = day11data(file)
    part1 = day11parts(data, 25)
    part2 = day11parts(data, 75)
    return build_response(part1, part2)

@app.route('/2016/2', methods=['POST'])
def day20162_rest():
    from p20162 import day20162_data, day20162_part1, day20162_part2
    file, success = get_file_from_request()
    if not success:
        return file, 400
    data = day20162_data(file)
    return build_response(*run_threads((day20162_part1, data), (day20162_part2, data)))

def run_threads(t1_args: tuple, t2_args: tuple) -> tuple:
    with concurrent.futures.ProcessPoolExecutor() as executor:
        t1 = executor.submit(*t1_args)
        t2 = executor.submit(*t2_args)
        return t1.result(), t2.result()

if __name__ == '__main__':
    app.run(debug=True)
