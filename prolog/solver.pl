:- use_module(cube_utils).

% Dynamic predicate to store states in the search tree
:- dynamic sphere/4.
% Dynamic predicate to store the number of states explored
:- dynamic states_explored/1.

/** 
 * solve(+State, -Path).
 * 
 * Finds the shortest path to solve the given scrambled state of the cube.
 * 
 * @param State The scrambled state of the cube represented as a string.
 * @param Path  The sequence of moves required to solve the cube.
 */
solve(State, Path) :-
    valid_state(State), % Ensure the input state is valid.
    reset_states, % Reset the state counter.
    retractall(sphere(_,_,_,_)), % Clear any existing states.
    build_sphere(scrambled, State, [], 0), % Build the scrambled state sphere.
    state_zero(Solved), % Get the solved state from the Prolog rules.
    build_sphere(solved, Solved, [], 0), % Build the solved state sphere.
    build_tree(solved, 0), % Build the solved state tree.
    build_tree(scrambled, 0), % Build the scrambled state tree.
    find_path(Path), % Find the shortest path between scrambled and solved states.
    apply_path(State, Path). % Apply the moves in the found path to the scrambled state.

/** 
 * build_sphere(+Origin, +State, +Moves, +Level).
 * 
 * Builds the state space for the cube from the given origin.
 * 
 * @param Origin Either 'scrambled' or 'solved'.
 * @param State  The state of the cube.
 * @param Moves  List of moves leading to this state.
 * @param Level  Depth of the state in the search tree.
 */
build_sphere(Origin, State, _, Level) :-
    sphere(Origin, State, _, Level), !. % Avoid duplicate states.

build_sphere(Origin, State, Moves, Level) :-
    increment_states, % Increment the state counter.
    assertz(sphere(Origin, State, Moves, Level)). % Store the state.

/**
 * increment_states.
 * 
 * Increments the global counter tracking the number of explored states.
 */
increment_states :-
    retract(states_explored(N)), % Retrieve the current state count.
    N1 is N + 1, % Increment the counter by 1.
    assert(states_explored(N1)). % Update the global counter.

/**
 * reset_states.
 * 
 * Resets the global state counter to zero.
 */
reset_states :-
    retractall(states_explored(_)), % Remove any existing state count.
    assert(states_explored(0)). % Set the counter to zero.

/** 
 * build_tree(+Origin, +Level).
 * 
 * Builds the search tree for the cube states from the given origin.
 * 
 * @param Origin Either 'scrambled' or 'solved'.
 * @param Level  Current depth in the search tree.
 */
build_tree(_, Level) :-
    Level >= 7, !. % Limit the depth of the search tree.

build_tree(_, _) :-
    sphere(scrambled, State, _, _),
    sphere(solved, State, _, _), !. % Stop if a connection is found.

build_tree(Origin, Level) :-
    sphere(Origin, State, PrevMoves, Level),
    move(Move), % Get a possible move.
    move_transform(State, Move, ChildState), % Apply the move to generate a new state.
    LevelNext is Level + 1,
    concat([Move], PrevMoves, NewMoves), % Append the move to the previous moves.
    build_sphere(Origin, ChildState, NewMoves, LevelNext), % Add the new state to the sphere.
    fail.

build_tree(Origin, Level) :-
    LevelNext is Level + 1,
    build_tree(Origin, LevelNext). % Continue building the tree at the next level.

/** 
 * find_path(-Path).
 * 
 * Finds the shortest path between the scrambled and solved states.
 * 
 * @param Path The sequence of moves required to solve the cube.
 */
find_path(Path) :-
    sphere(scrambled, State, ScrambledMoves, _),
    sphere(solved, State, SolvedMoves, _),
    reverse_all_moves(SolvedMoves, ReversedSolvedMoves),
    reverse(ScrambledMoves, ReversedScrambledMoves),
    concat(ReversedScrambledMoves, ReversedSolvedMoves, Path). % Combine the moves.

/** 
 * reverse_all_moves(+Moves, -ReversedMoves).
 * 
 * Reverses the direction of all moves in the given list.
 * 
 * @param Moves         List of original moves.
 * @param ReversedMoves List of moves with reversed directions.
 */
reverse_all_moves([], []).
reverse_all_moves([Move|Rest], [ReversedMove|ReversedRest]) :-
    reversal(Move, ReversedMove), % Get the reverse of the current move.
    reverse_all_moves(Rest, ReversedRest).

/** 
 * concat(+List1, +List2, -Result).
 * 
 * Concatenates two lists into a single list.
 * 
 * @param List1  The first list.
 * @param List2  The second list.
 * @param Result The concatenated list.
 */
concat([], List, List).
concat([Head|Tail], List, [Head|Result]) :-
    concat(Tail, List, Result).
