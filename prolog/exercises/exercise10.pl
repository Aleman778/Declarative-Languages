

% Chapter 1

parent(pam, bob).
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).

female(pam).
female(liz).
female(pat).
female(ann).

male(tom).
male(bob).
male(jim).

offspring(Y, X) :- parent(X, Y).

mother(X, Y) :- parent(X, Y), female(X).

father(X, Y) :- parent(X, Y), male(X).

grandparent(X, Z) :- parent(X, Y), parent(Y, Z).

sister(X, Y) :- parent(Z, X), parent(Z, Y), female(X).

haschild(X) :- parent(X, _).

happy(X) :- haschild(X).

hastwochildren(X) :- 
