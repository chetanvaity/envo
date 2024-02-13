from flask import Flask, jsonify
import random
import time

app = Flask(__name__)

@app.route('/dosomething')
def do_something():
    # Generate a random delay between 0.01 and 2 seconds
    delay = random.uniform(0.01, 2)
    # Wait for the random delay
    time.sleep(delay)
    # Return the delay in a JSON response
    return jsonify({"time_taken": delay})

if __name__ == '__main__':
    print("Starting 'dosomething' app...")
    app.run(host="0.0.0.0", port=5000, debug=True)
