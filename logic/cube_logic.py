import os
os.chdir("prolog") # Change to the directory containing the solver.pl file to solve the bug in the pyswipl library
from pyswip import Prolog

class CubeLogic:
    """
    Class to handle the logic of the 2x2 Rubik's Cube, including state representation,
    color validation, and solving through Prolog.
    """

    def __init__(self):
        """
        Initializes the CubeLogic class and sets up the initial cube state.
        Loads the Prolog solver file for solving and validation.

        Raises:
            RuntimeError: If the Prolog file cannot be loaded.
        """
        try:
            self.prolog = Prolog()
            self.prolog.consult("solver.pl")

        except Exception as e:
            raise RuntimeError(f"Error loading Prolog: {e}")

        self.reset_cube()
        self.update_states_explored()

    def reset_cube(self):
        """
        Resets the cube to the solved state.
        """
        self.state = {
            "U": [["white", "white"], ["white", "white"]],
            "D": [["yellow", "yellow"], ["yellow", "yellow"]],
            "F": [["green", "green"], ["green", "green"]],
            "B": [["blue", "blue"], ["blue", "blue"]],
            "L": [["orange", "orange"], ["orange", "orange"]],
            "R": [["red", "red"], ["red", "red"]],
        }

    def get_color(self, face, row, col):
        """
        Retrieves the color of a specific face, row, and column of the cube.

        Args:
            face (str): The face of the cube (e.g., "U", "D", "F", "B", "L", "R").
            row (int): The row index (0 or 1).
            col (int): The column index (0 or 1).

        Returns:
            str: The color at the specified position.
        """
        return self.state[face][row][col]

    def set_color(self, face, row, col, color):
        """
        Sets the color of a specific face, row, and column of the cube.

        Args:
            face (str): The face of the cube.
            row (int): The row index.
            col (int): The column index.
            color (str): The color to set.
        """
        self.state[face][row][col] = color

    def _get_order(self):
        """
        Returns the order of cube positions for Prolog string conversion.

        Returns:
            list: List of tuples representing the cube positions.
        """
        return [
            ("U", 1, 1), ("F", 0, 1), ("R", 0, 0),
            ("U", 0, 1), ("R", 0, 1), ("B", 0, 0),
            ("U", 0, 0), ("B", 0, 1), ("L", 0, 0),
            ("D", 0, 0), ("F", 1, 0), ("L", 1, 1),
            ("D", 1, 0), ("L", 1, 0), ("B", 1, 1),
            ("D", 1, 1), ("B", 1, 0), ("R", 1, 1),
            ("D", 0, 1), ("R", 1, 0), ("F", 1, 1),
        ]

    def get_cube_string(self):
        """
        Converts the current cube state into a Prolog-compatible string.

        Returns:
            str: A string representing the cube state, formatted for Prolog predicates.
        """
        order = self._get_order()
        cube_chars = [self.state[face][row][col][0] for face, row, col in order]
        return " ".join("".join(cube_chars[i:i+3]) for i in range(0, len(cube_chars), 3))

    def set_cube_from_string(self, cube_string):
        """
        Updates the cube state based on a Prolog-compatible string.

        Args:
            cube_string (str): A string representing the cube state, formatted for Prolog predicates.
        """
        order = self._get_order()
        color_map = {
            'w': 'white',
            'y': 'yellow',
            'r': 'red',
            'o': 'orange',
            'g': 'green',
            'b': 'blue'
        }

        cube_chars = list(cube_string.replace(" ", ""))

        if len(cube_chars) != len(order):
            raise ValueError("The provided cube string does not have the correct length.")

        for idx, (face, row, col) in enumerate(order):
            color = color_map.get(cube_chars[idx])
            if not color:
                raise ValueError(f"Invalid color code '{cube_chars[idx]}' in the input string.")
            self.state[face][row][col] = color

    def validate_colors(self):
        """
        Validates the cube's colors to ensure each appears the correct number of times.

        Returns:
            tuple: A boolean indicating validity and a list of errors (if any).
        """
        cube_str = self.get_cube_string()
        required_counts = {"white": 3, "orange": 3, "green": 3, "blue": 4, "red": 4, "yellow": 4}
        results = []

        for color, expected_count in required_counts.items():
            query = f"check_color_count('{cube_str}', '{color[0]}', {expected_count})"
            if not list(self.prolog.query(query)):
                results.append(color)

        if results:
            return False, results
        return True, []

    def is_solved(self):
        """
        Checks if the cube is in the solved state using the Prolog predicate `state_zero/1`.

        Returns:
            bool: True if the cube is solved, False otherwise.
        """
        cube_str = self.get_cube_string()
        query = f"state_zero('{cube_str}')"
        return bool(list(self.prolog.query(query)))

    def solve(self):
        """
        Solves the Rubik's Cube using Prolog and returns the list of moves.

        Returns:
            str: The solution as a sequence of moves, or None if no solution exists.
        """
        cube_str = self.get_cube_string()
        query = f"solve('{cube_str}', Path)"
        solution = list(self.prolog.query(query))

        if solution:
            path = solution[0]['Path']
            moves = (
                str(path)
                .replace("[", "")
                .replace("]", "")
                .replace("'", "")
                .replace('right_clockwise', 'R')
                .replace('right_counterclockwise', "R'")
                .replace('back_clockwise', 'B')
                .replace('back_counterclockwise', "B'")
                .replace('down_clockwise', 'D')
                .replace('down_counterclockwise', "D'")
            )
            self.update_states_explored()
            return moves
        else:
            return None

    def shuffle_cube(self):
        """
        Shuffles the cube to a random state.
        """
        query = "shuffle(Rand)"
        result = list(self.prolog.query(query))
        if result:
            rand_state = result[0]["Rand"]
            self.set_cube_from_string(rand_state)
        else:
            raise RuntimeError("Error generating a shuffled state.")

    def update_states_explored(self):
        """
        Update the number of states explored with the current count from Prolog.
        """
        query = "states_explored(Count)"
        result = list(self.prolog.query(query))
        if result:
            self.explored_states = result[0]["Count"]
        else:
            self.explored_states = 0

    def get_states_explored(self):
        """
        Returns the number of states explored during the last solve operation.

        Returns:
            int: The number of states explored.
        """
        return self.explored_states
