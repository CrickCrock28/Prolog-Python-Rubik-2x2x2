/* Utilities for representing the 2x2x2 Rubik's mini-cube, moves, and for applying moves.
 * 
 * This module provides functionality to handle the state of a 2x2x2 Rubik's Cube, apply transformations, 
 * validate states, generate random states, and determine move reversals. The cube's state is represented 
 * as an atom with space-separated encoded strings for each piece.
 */

:- module(cube_utils, [state_zero/1, % Get the solved state
                    apply_path/2, % Apply a series of moves to a state
                    shuffle/1, % Generate a shuffled state
                    move/1, % Define valid moves
                    reversal/2, % Define move reversals
                    move_transform/3, % Apply a move transformation to a state
                    valid_state/1, % Validate a cube state
                    check_color_count/3 % Validate color count in a state
                    ]).

/** <module> 2x2x2 Rubik's Cube Utilities
 */

/** state_zero(-State:atom).
 *
 * Provides the solved state of the 2x2x2 Rubik's Cube.
 *
 * @param State The solved state, represented as an atom.
 */
state_zero('wgr wrb wbo ygo yob ybr yrg'). % Pieces: UFR, URB, UBL, DFL, DLB, DBR, DRF

/** move(?Move:atom).
 *
 * Defines valid moves for the 2x2x2 Rubik's Cube.
 *
 * @param Move A valid move (e.g., down_clockwise, back_counterclockwise).
 */
move(down_clockwise).
move(down_counterclockwise).
move(right_clockwise).
move(right_counterclockwise).
move(back_clockwise).
move(back_counterclockwise).

/** reversal(+Move:atom, -Reversal:atom).
 *
 * Provides the reversal of a given move.
 *
 * @param Move The original move.
 * @param Reversal The move that reverses the original move.
 */
reversal(down_clockwise, down_counterclockwise).
reversal(down_counterclockwise, down_clockwise).
reversal(right_clockwise, right_counterclockwise).
reversal(right_counterclockwise, right_clockwise).
reversal(back_clockwise, back_counterclockwise).
reversal(back_counterclockwise, back_clockwise).

/** count_color(+State:atom, +Color:char, -Count:int).
 *
 * Counts the occurrences of a specific color in the cube state.
 *
 * @param State The cube state.
 * @param Color The color to count (e.g., 'w', 'r').
 * @param Count The number of occurrences of the color.
 */
count_color(State, Color, Count) :-
    atom_chars(State, Chars),
    include(=(Color), Chars, Filtered),
    length(Filtered, Count).

/** check_color_count(+State:atom, +Color:char, +ExpectedCount:int).
 *
 * Validates if a specific color appears the expected number of times in the state.
 *
 * @param State The cube state.
 * @param Color The color to validate.
 * @param ExpectedCount The expected count of the color.
 */
check_color_count(State, Color, ExpectedCount) :-
    count_color(State, Color, Count),
    Count =:= ExpectedCount.

/** valid_piece(+Piece:atom).
 *
 * Checks if a piece matches a solved piece (ignoring order).
 *
 * @param Piece A single piece of the cube.
 */
valid_piece(Piece) :-
    state_zero(Solved),
    atomic_list_concat(SolvedPieces, ' ', Solved),
    atom_chars(Piece, PieceChars),
    member(SolvedPiece, SolvedPieces),
    atom_chars(SolvedPiece, SolvedChars),
    msort(PieceChars, SortedPiece),
    msort(SolvedChars, SortedSolved),
    SortedPiece = SortedSolved.

/** valid_permutation(+State:atom).
 *
 * Validates if a cube state contains all valid pieces in any order.
 *
 * @param State The cube state.
 */
valid_permutation(State) :-
    atomic_list_concat(Pieces, ' ', State),
    state_zero(Solved),
    atomic_list_concat(SolvedPieces, ' ', Solved),
    maplist(atom_chars, Pieces, PiecesChars),
    maplist(msort, PiecesChars, SortedPiecesChars),
    maplist(atomic_list_concat, SortedPiecesChars, NormalizedPieces),
    maplist(atom_chars, SolvedPieces, SolvedPiecesChars),
    maplist(msort, SolvedPiecesChars, SortedSolvedPiecesChars),
    maplist(atomic_list_concat, SortedSolvedPiecesChars, NormalizedSolvedPieces),
    sort(NormalizedPieces, SortedPieces),
    sort(NormalizedSolvedPieces, SortedSolvedPieces),
    SortedPieces = SortedSolvedPieces.

/** valid_state(+State:atom).
 *
 * Checks if the given cube state is valid.
 *
 * @param State The cube state to validate.
 */
valid_state(State) :-
    check_color_count(State, 'w', 3), % 3 whites
    check_color_count(State, 'o', 3), % 3 oranges
    check_color_count(State, 'g', 3), % 3 greens
    check_color_count(State, 'b', 4), % 4 blues
    check_color_count(State, 'r', 4), % 4 reds
    check_color_count(State, 'y', 4), % 4 yellows,
    valid_permutation(State).

/** apply_path(+Start:atom, +Moves:list).
 *
 * Applies a series of moves to a cube state
 *
 * @param Start The starting cube state.
 * @param Moves The list of moves to apply.
 */
apply_path(Start, Moves) :-
    apply_path_(Start, Moves, 1).

apply_path_(Start, [Mv|Mvs], Step) :-
    move_transform(Start, Mv, Next),
    Step2 is Step + 1,
    !,
    apply_path_(Next, Mvs, Step2).

apply_path_(State, [], _) :-
    state_zero(State),
    !.
    
apply_path_(_, [], _) :-
    fail.

/** shuffle(+Rand:atom).
 *
 * Generates a randomly shuffled cube state.
 *
 * @param Rand The resulting shuffled state.
 */
shuffle(Rand) :-
    state_zero(Zero),
    shuffle(Zero, Rand).

/** shuffle(+Start:atom, -Rand:atom).
 *
 * Generates a shuffled cube state by applying a default of 30 random moves.
 *
 * @param Start The starting cube state.
 * @param Rand The resulting shuffled state.
 */
shuffle(Start, Rand) :-
    shuffle(Start, 30, Rand).

/** shuffle(+Start:atom, +Steps:int, -Rand:atom).
 *
 * Generates a shuffled cube state by applying random moves.
 *
 * @param Start The starting cube state.
 * @param Steps The number of random moves to apply.
 * @param Rand The resulting shuffled state.
 */
shuffle(Start, Steps, Rand) :-
    setof(Move, move(Move), Moves),
    shuffle(Start, Steps, Rand, Moves).

shuffle(State, Steps, Rand, Moves) :-
    Steps > 0,
    random_member(Move, Moves),
    move_transform(State, Move, Next),
    Steps_dec is Steps - 1,
    !,
    shuffle(Next, Steps_dec, Rand, Moves).
shuffle(State, 0, State, _).

/** move_transform(+In_state:atom, +Move:atom, -Out_state:atom).
 *
 * Transforms a cube state based on a move.
 *
 * @param In_state The input cube state.
 * @param Move The move to apply.
 * @param Out_state The resulting cube state after applying the move.
 */
move_transform(In_state, down_counterclockwise, Out_state) :- % D'
    map_state_listed(In_state, As),
    As = [[A1,C1,E1], [A2,E2,D2], [A3,D3,B3],
          [F4,C4,B4], [F5,B5,D5], [F6,D6,E6], [F7,E7,C7]],
    Zs = [[A1,C1,E1], [A2,E2,D2], [A3,D3,B3],
          [F7,E7,C7], [F4,C4,B4], [F5,B5,D5], [F6,D6,E6]],
    map_state_listed(Out_state, Zs).
move_transform(In_state, down_clockwise, Out_state) :- % D
    map_state_listed(In_state, As),
    As = [[A1,C1,E1], [A2,E2,D2], [A3,D3,B3],
          [F4,C4,B4], [F5,B5,D5], [F6,D6,E6], [F7,E7,C7]],
    Zs = [[A1,C1,E1], [A2,E2,D2], [A3,D3,B3],
          [F5,B5,D5], [F6,D6,E6], [F7,E7,C7], [F4,C4,B4]],
    map_state_listed(Out_state, Zs).
move_transform(In_state, right_counterclockwise, Out_state) :- % R'
    map_state_listed(In_state, As),
    As = [[A1,C1,E1], [A2,E2,D2], [A3,D3,B3],
          [F4,C4,B4], [F5,B5,D5], [F6,D6,E6], [F7,E7,C7]],
    Zs = [[D2,A2,E2], [D6,E6,F6], [A3,D3,B3],
          [F4,C4,B4], [F5,B5,D5], [C7,F7,E7], [C1,E1,A1]],
    map_state_listed(Out_state, Zs).
move_transform(In_state, right_clockwise, Out_state) :- % R
    map_state_listed(In_state, As),
    As = [[A1,C1,E1], [A2,E2,D2], [A3,D3,B3],
          [F4,C4,B4], [F5,B5,D5], [F6,D6,E6], [F7,E7,C7]],
    Zs = [[C7,F7,E7], [C1,E1,A1], [A3,D3,B3],
          [F4,C4,B4], [F5,B5,D5], [D2,A2,E2], [D6,E6,F6]],
    map_state_listed(Out_state, Zs).
move_transform(In_state, back_counterclockwise, Out_state) :- % B'
    map_state_listed(In_state, As),
    As = [[A1,C1,E1], [A2,E2,D2], [A3,D3,B3],
          [F4,C4,B4], [F5,B5,D5], [F6,D6,E6], [F7,E7,C7]],
    Zs = [[A1,C1,E1], [B3,A3,D3], [B5,D5,F5],
          [F4,C4,B4], [E6,F6,D6], [E2,D2,A2], [F7,E7,C7]],
    map_state_listed(Out_state, Zs).
move_transform(In_state, back_clockwise, Out_state) :- % B
    map_state_listed(In_state, As),
    As = [[A1,C1,E1], [A2,E2,D2], [A3,D3,B3],
          [F4,C4,B4], [F5,B5,D5], [F6,D6,E6], [F7,E7,C7]],
    Zs = [[A1,C1,E1], [E6,F6,D6], [E2,D2,A2],
          [F4,C4,B4], [B3,A3,D3], [B5,D5,F5], [F7,E7,C7]],
    map_state_listed(Out_state, Zs).

/** map_state_listed(?Atom:atom, ?List:list).
 *
 * Converts between an atom representation and a list representation of a cube state.
 *
 * @param Atom The atom representation of the state.
 * @param List The list representation of the state.
 */
map_state_listed(Atom, List) :-
    ground(List),
    var(Atom),
    maplist(atomic_list_concat, List, As),
    atomic_list_concat(As, ' ', Atom),
    !.
map_state_listed(Atom, List) :-
    var(List),
    ground(Atom),
    atomic_list_concat(As, ' ', Atom),
    maplist(atom_chars, As, List).
