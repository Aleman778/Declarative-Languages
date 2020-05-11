% Author Alexander Mennborg

% Program Definitions:
% State is defined by the robot and 3 lists of items, one for each room i.e. state(Robot, Room1, Room2, Room3).
% Robot is defined by its location (the current room) and a list containing at most 2 items (inventory) i.e. robot(CurrentRoom, [ItemA, ItemB]).
% The items are defined by the following atoms: package, brass_key, steel_key, nothing.
% The scene has 3 rooms and are defined by the following atoms: room1, room2, room3.


robot_has_item(X, [ItemA, ItemB]) :- ItemA == X; ItemB == X.

robot_pickup_item(X, [nothing, Y], [X, Y]) :- !.
robot_pickup_item(X, [Y, nothing], [Y, X]) :- !.

robot_drop_item(X, [X, Y], [nothing, Y]) :- not(X == nothing).
robot_drop_item(X, [Y, X], [Y, nothing]) :- not(X == nothing).

% Pickup action: picks up an item in the robots current room.
move(state(robot(room1, Inventory1), L1, Room2, Room3),
     pickup(Item),
     state(robot(room1, Inventory2), L2, Room2, Room3)) :- select(Item, L1, L2), robot_pickup_item(Item, Inventory1, Inventory2).

move(state(robot(room2, Inventory1), Room1, L1, Room3),
     pickup(Item),
     state(robot(room2, Inventory2), Room1, L2, Room3)) :- select(Item, L1, L2), robot_pickup_item(Item, Inventory1, Inventory2).

move(state(robot(room3, Inventory1), Room1, Room2, L1),
     pickup(Item),
     state(robot(room3, Inventory2), Room1, Room2, L2)) :- select(Item, L1, L2), robot_pickup_item(Item, Inventory1, Inventory2).


% Move action: moves the robot from its current room goto another room.
move(state(robot(room1, Inventory), Room1, Room2, Room3), 
     move(room2), 
     state(robot(room2, Inventory), Room1, Room2, Room3)) :- robot_has_item(steel_key, Inventory).

move(state(robot(room1, Inventory), Room1, Room2, Room3), 
     move(room3), 
     state(robot(room3, Inventory), Room1, Room2, Room3)) :- robot_has_item(brass_key, Inventory).
                                                                                                  
move(state(robot(room2, Inventory), Room1, Room2, Room3), 
     move(room1), 
     state(robot(room1, Inventory), Room1, Room2, Room3)) :- robot_has_item(steel_key, Inventory).

move(state(robot(room3, Inventory), Room1, Room2, Room3), 
     move(room1), 
     state(robot(room1, Inventory), Room1, Room2, Room3)) :- robot_has_item(brass_key, Inventory).


% Drop action: robot drops one if its item into the current room.
move(state(robot(room1, Inventory1),  Room1, Room2, Room3),
     drop(Item),
     state(robot(room1, Inventory2), [Item|Room1], Room2, Room3)) :- robot_drop_item(Item, Inventory1, Inventory2).

move(state(robot(room2, Inventory1),  Room1, Room2, Room3),
     drop(Item),
     state(robot(room2, Inventory2), Room1, [Item|Room2], Room3)) :- robot_drop_item(Item, Inventory1, Inventory2).

move(state(robot(room3, Inventory1),  Room1, Room2, Room3),
     drop(Item),
     state(robot(room3, Inventory2), Room1, Room2, [Item|Room3])) :- robot_drop_item(Item, Inventory1, Inventory2).


solve_and_display(State, N) :-
    solveR(State, N, Trace), !, display_trace(Trace).


% The solved state is when the package is found in room 2 (member of room 2s items), N has to be larger than 0.
solveR(state(_, _, Room2, _), N, [done | []]) :- member(package, Room2), N > 0.

solveR(State1, N, [Move| Trace2]) :-
    N > 0,
    move(State1, Move, State2),
    solveR(State2, N - 1, Trace2).

% Printing the trace to solve the problem.
display_trace(Trace) :-
    length(Trace, Len),
    write("Solution (Solved in "), write(Len), write(" steps):"), nl,
    write_trace(1, Trace).

write_trace(_, []).

write_trace(Step, [T|Trace]) :-
    write(Step), write(". "),
    write_step(T),
    NStep is Step + 1,
    write_trace(NStep, Trace), !.

write_step(move(Room)) :-
    write("Move to: "), write(Room), nl.

write_step(pickup(Item)) :-
    write("Pick up item: "), write(Item), nl.

write_step(drop(Item)) :-
    write("Drop item: "), write(Item), nl.

write_step(done) :-
    write("Done!"), nl.

% Example from the lab assignment 
example_state(state(robot(room1, [nothing, nothing]), [steel_key], [brass_key], [package])).

solve_example_state() :-
    example_state(State),
    solve_and_display(State, 13).
