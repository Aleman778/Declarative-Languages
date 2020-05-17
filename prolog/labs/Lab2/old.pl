% Author: Alexander Mennborg


% Structure: sublist(Sublist: [t], Size: Int, I: Int, J; Int)
create_sublist(List, I, SubList) :- 
    length(List, Len), size(List, Size), J is I + Len,
    SubList = sublist(List, Size, I, J).


sublist_equals([], []).
sublist_equals([X|Xs], [Y|Ys]) :-
    X == Y,
    sublist_equals(Xs, Ys), !.

sublist_equals(sublist(Xs, _, Xi, Xj), sublist(Ys, _, Yi, Yj)) :-
    Xj - Xi == Yj - Yi,
    sublist_equals(Xs, Ys).


not_in_set(_, []) :- true, !.
not_in_set(X, [Y|Ys]) :-
    not(sublist_equals(X, Y)),
    not_in_set(X, Ys), !.


% Size is defined as the sum of all elements in a list or sublist.
size(List, Size) :-
    sum_list(List, Size).
size(sublist(_, Size, _, _), Size).


% Returns a list of last indices of sublists, and also to get them unique use that procedure instead.
get_last_indices([], []).
get_last_indices([sublist(_, _, _, J)|Xs], [J|Ys]) :-
    get_last_indices(Xs, Ys).
get_last_unique_indices(Xs, Ys) :-
    get_last_indices(Xs, XsInt),
    list_to_set(XsInt, Ys).
    

% Compares two sublists and determines the smallest one first by size and then by length.
% Returns true if the left sublist is smaller than the right and vice versa.
compare(sublist(_, SizeX, Xi, Xj), sublist(_, SizeY, Yi, Yj)) :-
    SizeX < SizeY -> true, !;
    SizeX > SizeY -> false, !;
    LenX is Xj - Xi, LenY is Yj - Yi, LenX =< LenY, !.


% Concatenates consecutive sublists from the given set of sublists, I know very confusing!
% We essentially want to find all possible combinations of the current head element concatenated
% with each sublist in our smallest k set, there may be the given scenario
% SubList = [-3, 1, -2], Idx = 0, K = 1, Set = [[-2]], we want the set to be Set = [-3, 1, -2] but
% there is a 1 in the way that we need to consider.
append_consecutive(_, _, _, [], Set, Set).
append_consecutive(List, Idx, K, [J | Js], Set, Set2) :-
    J2 is J - Idx - 1,
    findall(X, (nth0(I, List, X), I =< J2), Xs),
    create_sublist(Xs, Idx, SubList),
    append_k_smallest(SubList, Set, K, SetInt),
    append_consecutive(List, Idx, K, Js, SetInt, Set2).

append_consecutive_k_smallest(List, Idx, K, Set, Set2) :-
    get_last_unique_indices(Set, Js),
    append_consecutive(List, Idx, K, Js, Set, Set2).

% Append to list while keeping items ordered and fixed size, k.
append_k_smallest(X, L1, K, L2) :-
    not_in_set(X, L1),
    (length(L1, Len),
    Len >= K,
    append_smallest(X, L1, L3),
    remove_last(L3, L2), !;
    append_smallest(X, L1, L2), !); L2 = L1, !.


% Appending a list of entries with smallest `size` in increasing order.
append_smallest(X, [], [X]).

append_smallest(X, [Y | L1], L2) :-
    compare(X, Y),
    append_smallest(Y, L1, Last), list_to_set([X|Last], L2), !;
    append_smallest(X, L1, Last), list_to_set([Y|Last], L2), !.
    

% Removes the last element in the list.
remove_last([X|Xs], Ys) :-
    remove_last_prev(Xs, Ys, X).


% Intermediate step in removing the last element, 
% we lag behind one element and insert previous element into Ys.
remove_last_prev([], [], _).
remove_last_prev([X1|Xs], [X0|Ys], X0) :-
    remove_last_prev(Xs, Ys, X1).

% In this case where we only have a single element we could try to insert either
% a sublist containing that element or just an empty sublist.
smallest_k_set(sublist([X | []], Size, I, J), K, Set, Set2) :- 
    append_k_smallest(sublist([X], Size, I, J), Set, K, Set2).


% In this case we can choose to include the head or not and recursively find the smallest k set on
% the tail of the list.
smallest_k_set(sublist([X | Xs], Size, I, J), K, Set, Set2) :-
    SizeXs is Size - X,
    I2 is I + 1,
    XSubList = sublist([X], X, I, I2),
    XsSubList = sublist(Xs, SizeXs, I2, J),
    smallest_k_set(XsSubList, K, Set, SetInt),
    append_consecutive_k_smallest([X | Xs], I, K, SetInt, SetInt2),
    append_k_smallest(XSubList, SetInt2, K, Set2).
    

% If an empty list is provided then throw an error.
smallest_k_set([], _, _) :- 
    throw("Smallest K set does not accept an empty list.").


% The starting call with a list of elements, integer K and 
% result of sets containing k smallest sublists are stored in Set.
smallest_k_set(List, K, Set) :- 
    create_sublist(List, 0, SubList),
    smallest_k_set(SubList, K, [], Set).

%%
%% Test cases from Haskel lab 1.
%%


run_test_cases() :-
    write("Test case 1:\n-------------------------------------------------\n"),
    test_case_1(),
    write("\n\nTest case 2:\n-------------------------------------------------\n"),
    test_case_2(),
    write("\n\nTest case 3:\n-------------------------------------------------\n"),
    test_case_3().


test_case_1() :-
    test_case_1_list(1, List),
    smallest_k_set(List, 15, Set), display_k_set(List, Set).

    
test_case_1_list(N, [X | Xs]) :-
    N2 is N + 1,
    (N < 100, test_case_1_list(N2, Xs), !; Xs = [], !),
    (mod(N, 2) =:= 0, X is N, !; X is -N, !).


test_case_2() :-
    List = [24, -11, -34, 42, -24, 7, -19, 21],
    smallest_k_set(List, 6, Set), display_k_set(List, Set).

    
test_case_3() :-
    List = [3, 2, -4, 3, 2, -5, -2, 2, 3, -3, 2, -5, 6, -2, 2, 3],
    smallest_k_set(List, 8, Set), display_k_set(List, Set).


%%
%% Displaying the k smallest sets.
%%


% Displays the results in a nice format.
display_k_set(List, Set) :-
    write("Entire list: "),
    list_string(List, ListStr),
    write(ListStr), nl, nl,
    create_matrix(Set, Matrix),
    calculate_spacing(Matrix, Spacing),
    display_matrix(Matrix, Spacing).


% Displays the given matrix.
display_matrix([], _).
display_matrix([row(S1, S2, S3, S4) | Matrix], spacing(Spc1, Spc2, Spc3)) :-
    write_pushed(S1, Spc1), 
    write_pushed(S2, Spc2), 
    write_pushed(S3, Spc3), 
    write("  "), write(S4), nl,
    display_matrix(Matrix, spacing(Spc1, Spc2, Spc3)).


write_pushed(Str, N) :-
    string_length(Str, StrLen),
    NumSpaces is N - StrLen,
    write_spaces(NumSpaces),
    write(Str).
    

write_spaces(N) :-
    N > 0, write(" "), N2 is N - 1, write_spaces(N2), !;
    !.

% Displays the sublist in string format.
list_string_int([], "]").
list_string_int([X | Xs], S) :- 
    number_string(X, S1),
    string_concat(", ", S1, S2),
    list_string_int(Xs, S3),
    string_concat(S2, S3, S).
list_string([X| Xs], S) :-
    number_string(X, S1),
    list_string_int(Xs, S2),
    string_concat(S1, S2, S3),
    string_concat("[", S3, S).
    
    
% Create a row in the matrix.
create_header_row(row("size", "i", "j", "sublist")).
create_row(sublist(SubList, Size, I, J), row(S1, S2, S3, S4)) :-
    number_string(Size, S1),
    number_string(I, S2),
    number_string(J, S3),
    list_string(SubList, S4).


% Creates the matrix with information about the given k smallest set.
create_matrix_int([], []).
create_matrix_int([X | Xs], [Row | Matrix]) :-
    create_row(X, Row),
    create_matrix_int(Xs, Matrix).
create_matrix([X | Xs], [HeaderRow | [Row | Matrix]]) :-
    create_header_row(HeaderRow),
    create_row(X, Row),
    create_matrix_int(Xs, Matrix).


% Calculate the maximum number of spacing needed to display for each column 3 column, the forth
% column is left aligned so it does not need to be considered.
calculate_spacing([], S, S) :- !.
calculate_spacing([Row | Xs], spacing(Y1, Y2, Y3), Spacing) :-
    get_spacing(Row, spacing(X1, X2, X3)),
    (X1 > Y1, S1 is X1, !; S1 is Y1, !),
    (X2 > Y2, S2 is X2, !; S2 is Y2, !),
    (X3 > Y3, S3 is X3, !; S3 is Y3, !),
    calculate_spacing(Xs, spacing(S1, S2, S3), Spacing).
calculate_spacing(Matrix, Spacing) :-
    calculate_spacing(Matrix, spacing(0, 0, 0), Spacing).

    
% Returns the spacing 
get_spacing(row(Size, I, J, _), spacing(T1, T2, T3)) :-
    string_length(Size, S1), T1 is S1 + 2,
    string_length(I, S2),    T2 is S2 + 2,
    string_length(J, S3),    T3 is S3 + 2.

