% Author: Alexander Mennborg


% Structure: sublist(Sublist: [t], Size: Int, I: Int, J; Int)
create_sublist(List, I, SubList) :- 
    length(List, Len), size(List, Size), J is I + Len,
    SubList = sublist(List, Size, I, J).


% Size is defined as the sum of all elements in a list or sublist.
size(List, Size) :-
    sum_list(List, Size).
size(sublist(_, Size, _, _), Size).


% Compares two sublists and determines the smallest one first by size and then by length.
% Returns true if the left sublist is smaller than the right and vice versa.
compare(sublist(_, SizeX, Xi, Xj), sublist(_, SizeY, Yi, Yj)) :-
    LenX is Xj - Xi, LenY is Yj - Yi,
    SizeX =< SizeY, SizeX =:= SizeY, 
    LenX < LenY; SizeX < SizeY.


% Concatenates consecutive sublists from the given set of sublists, I know very confusing!
% We essentially want to find all possible combinations of the current head element concatenated
% with each sublist in our smallest k set, there may be the given scenario
% SubList = [-3, 1, -2], Idx = 0, K = 1, Set = [[-2]], we want the set to be Set = [-3, 1, -2] but
% there is a 1 in the way that we need to consider.
append_consecutive(_, _, _, [], Set, Set).
append_consecutive(List, Idx, K, [J | Js], Set, Set2) :-
    J2 is J - 1,
    findall(X, (nth0(I, List, X), I =< J2), Xs),
    create_sublist(Xs, Idx, SubList),
    append_k_smallest(SubList, Set, K, SetInt),
    append_consecutive(List, Idx, K, Js, SetInt, Set2).

append_consecutive_k_smallest(List, Idx, K, Set, Set2) :-
    setof(J, I^nth0(I, Set, sublist(_, _, _, J)), Js), !,
    append_consecutive(List, Idx, K, Js, Set, Set2).


% Append to list while keeping items ordered and fixed size, k.
append_k_smallest(X, L1, K, L2) :-
    length(L1, Len),
    Len >= K,
    append_smallest(X, L1, L3),
    remove_last(L3, L2), !;
    append_smallest(X, L1, L2).

% Appending a list of entries with smallest `size` in increasing order.
append_smallest(X, [], [X]).
append_smallest(X, [Y | L1], L2) :-
    compare(X, Y),
    append_smallest(Y, L1, Last), L2 = [X|Last], !;
    append_smallest(X, L1, Last), L2 = [Y|Last], !.
    

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
    append_consecutive_k_smallest([X | Xs], I2, K, SetInt, SetInt2),
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
%% Displaying the k smallest sets.
%%


% Displays the results in a nice format.
display_k_set(Set) :-
    create_matrix(Set, Matrix),
    calculate_spacing(Matrix, Spacing),
    display_matrix(Matrix, Spacing).


% Displays the given matrix.
display_matrix([], _).
display_matrix([[S1, S2, S3, S4] | Matrix], spacing(Spc1, Spc2, Spc3)) :-
    write_pushed(S1, Spc1), 
    write_pushed(S2, Spc2), 
    write_pushed(S3, Spc3), 
    write_pushed(S4, 2), nl,
    display_matrix(Matrix).


write_pushed(Str, N) :-
    string_length(Str, StrLen),
    NumSpaces is N - StrLen,
    write_spaces(NumSpaces),
    write(Str).
    

write_spaces(0).
write_spaces(N) :-
    write(" "), N2 is N - 1, write_spaces(N2).



% Displays the sublist in string format.
display_sublist_int([], "]").
display_sublist_int([X | Xs], S) :- 
    number_string(X, S1),
    string_concat(", ", S1, S2),
    display_sublist_int(Xs, S3),
    string_concat(S2, S3, S).
display_sublist([X| Xs], S) :-
    number_string(X, S1),
    display_sublist_int(Xs, S2),
    string_concat(S1, S2, S3),
    string_concat("[", S3, S).
    
    
% Create a row in the matrix.
create_header_row(row("size", "i", "j", "sublist")).
create_row(sublist(SubList, Size, I, J), row(S1, S2, S3, S4)) :-
    number_string(Size, S1),
    number_string(I, S2),
    number_string(J, S3),
    display_sublist(SubList, S4).


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

