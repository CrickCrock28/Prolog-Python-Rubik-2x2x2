import time
from logic.cube_logic import CubeLogic

def test_resolution(cube_logic, num_tests=100):
    """
    Tests the cube-solving algorithm on random configurations and collects metrics.

    Args:
        cube_logic (CubeLogic): Instance of the cube logic system.
        num_tests (int): Number of random configurations to test.

    Returns:
        dict: Collected metrics, including average solving time, explored depth, and explored states.
    """
    results = {
        "times": [],
        "depths": [],
        "states_explored": [],
        "solved_correctly": 0,
    }

    for i in range(num_tests):
        print(f"Test {i + 1}/{num_tests}...")

        cube_logic.shuffle_cube()

        try:
            start_time = time.time()
            solution = cube_logic.solve()
            elapsed_time = time.time() - start_time

            results["times"].append(elapsed_time)
            results["solved_correctly"] += 1
            results["depths"].append(len(solution.split()) / 2)
            results["states_explored"].append(cube_logic.get_states_explored())

        except Exception as e:
            print(f"Error during test {i + 1}: {e}")

    return results

def analyze_results(results):
    """
    Analyzes test results and calculates average metrics.

    Args:
        results (dict): Collected test results.

    Returns:
        dict: Calculated average metrics.
    """
    num_tests = len(results["times"])
    average_time = sum(results["times"]) / num_tests
    average_depth = sum(results["depths"]) / num_tests
    average_states_explored = sum(results["states_explored"]) / num_tests

    print("\n--- TEST RESULTS ---")
    print(f"Total number of tests: {num_tests}")
    print(f"Average solving time: {average_time:.4f} seconds")
    print(f"Average explored depth: {average_depth:.2f}")
    print(f"Average states explored: {average_states_explored:.2f}")
    print(f"Percentage of correct solutions: {(results['solved_correctly'] / num_tests) * 100:.2f}%")

    return {
        "average_time": average_time,
        "average_depth": average_depth,
        "average_states_explored": average_states_explored,
        "correct_solutions_percentage": (results["solved_correctly"] / num_tests) * 100,
    }

def main():
    """
    Main function to initialize the cube logic system, run tests, and analyze results.
    """
    cube_logic = CubeLogic()
    NUM_TESTS = 1000
    results = test_resolution(cube_logic, NUM_TESTS)
    analyze_results(results)

if __name__ == "__main__":
    main()
