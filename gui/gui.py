import tkinter as tk
from tkinter import messagebox
from logic.cube_logic import CubeLogic

class CubeGUI:
    """
    Class to manage the graphical interface for the 2x2 Rubik's Cube using tkinter.
    """
    def __init__(self, master):
        """
        Initializes the graphical interface for the Rubik's Cube.

        Args:
            master (tk.Tk): The root window for the tkinter application.
        """
        self.master = master
        self.master.title("2x2 Rubik's Cube")

        self.logic = CubeLogic()
        self.current_color = "white"
        self.colors_palette = ["white", "yellow", "green", "blue", "orange", "red"]

        self.immutable_positions = {
            "U": [(1, 0)],
            "F": [(0, 0)],
            "L": [(0, 1)]
        }  # White-Orange-Green corner is immutable

        self.draw_interface()

    def draw_interface(self):
        """
        Draws the graphical interface for the cube and the color palette.
        """
        for widget in self.master.winfo_children():
            widget.destroy()

        cube_frame = tk.Frame(self.master)
        cube_frame.grid(row=0, column=0, padx=10, pady=10)

        positions = {
            "U": (0, 1), "L": (1, 0), "F": (1, 1), "R": (1, 2), "B": (1, 3), "D": (2, 1),
        }

        for face, (row_offset, col_offset) in positions.items():
            for r in range(2):
                for c in range(2):
                    color = self.logic.get_color(face, r, c)
                    is_immutable = face in self.immutable_positions and (r, c) in self.immutable_positions[face]
                    btn = tk.Button(
                        cube_frame,
                        bg=color,
                        width=5,
                        height=2,
                        state=tk.DISABLED if is_immutable else tk.NORMAL,
                        relief=tk.SUNKEN if is_immutable else tk.RAISED,
                    )
                    if not is_immutable:
                        btn.configure(command=lambda f=face, row=r, col=c: self.change_color(f, row, col))
                    btn.grid(row=row_offset * 2 + r, column=col_offset * 2 + c)

        self.draw_palette()

    def draw_palette(self):
        """
        Draws the selectable color palette for the cube.
        """
        palette_frame = tk.Frame(self.master)
        palette_frame.grid(row=1, column=0, padx=10, pady=10)

        tk.Label(palette_frame, text="Pick a color:").grid(row=0, column=0, columnspan=len(self.colors_palette))

        for i, color in enumerate(self.colors_palette):
            btn = tk.Button(
                palette_frame,
                bg=color,
                width=5,
                height=2,
                command=lambda col=color: self.select_color(col),
                relief=tk.SUNKEN if color == self.current_color else tk.RAISED,
            )
            btn.grid(row=1, column=i)

        tk.Button(self.master, text="Reset", command=self.reset_cube, width=10, height=2).grid(row=2, column=0, pady=10)
        tk.Button(self.master, text="Shuffle", command=self.shuffle_cube, width=10, height=2).grid(row=3, column=0, pady=10)
        tk.Button(self.master, text="Solve", command=self.solve_cube, width=10, height=2).grid(row=4, column=0, pady=10)

    def change_color(self, face, row, col):
        """
        Changes the color of the selected piece of the cube.

        Args:
            face (str): The face of the cube.
            row (int): The row index of the piece.
            col (int): The column index of the piece.
        """
        self.logic.set_color(face, row, col, self.current_color)
        self.draw_interface()

    def select_color(self, color):
        """
        Updates the currently selected color.

        Args:
            color (str): The color selected from the palette.
        """
        self.current_color = color
        self.draw_interface()

    def reset_cube(self):
        """
        Resets the cube colors to the initial solved state.
        """
        self.logic.reset_cube()
        self.draw_interface()

    def shuffle_cube(self):
        """
        Shuffles the cube to a random state.
        """
        try:
            self.logic.shuffle_cube()
            self.draw_interface()
        except RuntimeError as e:
            messagebox.showerror("Error", str(e))

    def solve_cube(self):
        """
        Validates the cube's colors, checks if it is solved, and attempts to solve it using CubeLogic.
        Displays the result or any errors in a message box.
        """
        # Check if the cube is already solved
        if self.logic.is_solved():
            messagebox.showinfo("Cube Solved", "The cube is already solved!")
            return

        # Validate the cube's colors
        is_valid, errors = self.logic.validate_colors()

        if not is_valid:
            messagebox.showerror("Error", f"Invalid cube configuration. The following colors are not inserted exactly 4 times:\n{', '.join(errors)}")
            return

        # Attempt to solve the cube
        solution = self.logic.solve()
        if solution:
            messagebox.showinfo("Solution", f"Moves to solve:\n{solution}")
        else:
            messagebox.showerror("Error", "The cube configuration is invalid.")
