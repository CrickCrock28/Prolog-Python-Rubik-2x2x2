from gui.gui import CubeGUI
import tkinter as tk

def main():
    """
    Entry point for the 2x2 Rubik's Cube application.

    Initializes the tkinter root window and starts the graphical interface for interacting with the Rubik's Cube using the CubeGUI class.
    """
    root = tk.Tk()
    app = CubeGUI(root)
    root.mainloop()

if __name__ == "__main__":
    main()
