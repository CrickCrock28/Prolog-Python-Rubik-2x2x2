# 2x2 Rubik's Cube Solver

This project implements a system for automatically solving the 2x2 Rubik's Cube using a combination of Python and Prolog. The system allows you to:
- Insert a cube configuration with a graphical interface.
- Generate valid random configurations of the cube.
- Compute the optimal sequence of moves to solve the cube using a bidirectional search algorithm.

## Requirements
- Python 3.x
- SWI-Prolog
- Python library `pyswip`

## Running the Project
To run the application, execute:
```bash
python app.py
```

## Documentation
The complete project documentation is available in PDF format at:

```
docs/docs.pdf
```

For automatically generated documentation:
- **Python**: Available in `docs/pydocs/index.html`
- **Prolog**: Available in `docs/pldocs/solver.html` and `docs/pldocs/cube_utils.html`

## Author
This project was developed by Luca Ardito, student ID 777818 at the University of Bari (Uniba). It was carried out during the 2024-2025 academic year for the Knowledge Engineering course, taught by Professor Nicola Fanizzi. For any inquiries, contact [luca.ardito@studenti.uniba.it](mailto:luca.ardito@studenti.uniba.it).
