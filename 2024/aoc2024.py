from flask import Flask, jsonify, request
from flask_cors import CORS

app = Flask(__name__)
CORS(app)

def build_response(part1, part2):
    return jsonify({'part1': part1, 'part2': part2})

def get_file_from_request():
    if 'file' not in request.files:
        print ('No file part in the request')
        return jsonify({'error': 'No file part in the request'}), False
    file = request.files['file']
    if file.filename == '':
        print ('No selected file')
        return jsonify({'error': 'No selected file'}), False
    return file, True

@app.route('/2024/1', methods=['POST'])
def post_sum_diffs():
    from day1funcs import sum_diffs, sum_similar, day1_data
    file, success = get_file_from_request()
    if not success:
        return file, 400
    left, right = day1_data(file)
    part1 = sum_diffs(left, right)
    part2 = sum_similar(left, right)
    return build_response(part1, part2)

@app.route('/2024/2', methods=['POST'])
def day_2_rest():
    from day2funcs import day2_data, count_safe, count_kinda_safe
    file, success = get_file_from_request()
    if not success:
        return file, 400
    list_of_lists = day2_data(file)
    part1 = count_safe(list_of_lists)
    part2 = count_kinda_safe(list_of_lists)
    return build_response(part1, part2)

if __name__ == '__main__':
    app.run(debug=True)
