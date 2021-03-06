% Author: Alexander Mennborg


% Structure: sublist(Sublist: [t], Size: Int, I: Int, J; Int)


% Creates all possible combinations of sublists using the given list.
create_sublists(L1, L2) :-
    length(L1, Len), Len2 is Len + 1,
    findall((I, J), (between(1, Len2, I), between(1, Len2, J), I =< J), Indices),
    list_to_set(Indices, Indices2),
    create_sublists(L1, Indices2, L2).
create_sublists(_, [], []).
create_sublists(L, [(I, J)| Indices], [sublist(L2, Size, I, J)| Xs]) :-
    findall(X, (between(I, J, K), nth1(K, L, X)), L2),
    size(L2, Size),
    create_sublists(L, Indices, Xs), !.


% Size is defined as the sum of all elements in a list or sublist.
size(List, Size) :-
    sum_list(List, Size).
size(sublist(_, Size, _, _), Size).
    

% Compares two sublists and determines the smallest one first by size and then by length.
% Returns true if the left sublist is smaller than the right and vice versa.
compare(sublist(_, SizeX, Xi, Xj), sublist(_, SizeY, Yi, Yj)) :-
    SizeX < SizeY -> true, !;
    SizeX > SizeY -> false, !;
    Xi > Yi, Xj > Yj, !.
 

% Merge sort implementation.
merge_sprt([], []).
merge_sort([X], [X]).
merge_sort(L1, L2) :- 
    split_list(L1, Xs, Ys),
    merge_sort(Xs, Xs2),
    merge_sort(Ys, Ys2),
    merge_lists(Xs2, Ys2, L2).
    

% Splits list L into two sublists Xs and Ys, used in merge sort.
split_list(L, Xs, Ys) :-
    append(Xs, Ys, L),
    length(Xs, N1), length(Ys, N2),
    N is N1 - N2, N >= 0, N =< 1, !.


% Merges two lists into one list, used in merge sort.
merge_lists(Xs, [], Xs).
merge_lists([], Ys, Ys).
merge_lists([X|Xs], [Y|Ys], [Z|Zs]) :-
    compare(X, Y),
    Z = X, merge_lists(Xs, [Y|Ys], Zs), !;
    Z = Y, merge_lists([X|Xs], Ys, Zs), !.


% If an empty list is provided then throw an error.
smallest_k_set([], _, _) :- 
    throw("Smallest K set does not accept an empty list.").


% The starting call with a list of elements, integer K and 
% result of sets containing k smallest sublists are stored in Set.
smallest_k_set(List, K, Set) :- 
    create_sublists(List, Set2),
    merge_sort(Set2, Set3),
    findall(X, (nth0(I, Set3, X), I < K), Set).



%%
%% Test cases from Haskel lab 1.
%%


run_test_cases() :-
    write("Test case 1:\n-------------------------------------------------\n"),
    test_case_1(),
    write("\n\nTest case 2:\n-------------------------------------------------\n"),
    test_case_2(),
    write("\n\nTest case 3:\n-------------------------------------------------\n"),
    test_case_3(), !.


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

