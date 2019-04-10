% 531 Prolog
% Assessed Exercise 1
% formulas.pl


% Write your answers to the exercise here
:- consult(support).

% Task 1: wff(+F) %+F means ground -F means variable
% wff(F) holds when F is a (well-formed) formula.

% If formula A is not ground we want to fail without further backtracking.
wff(A)            :-
  \+ ground(A), !, fail.

wff(A)            :-
  logical_atom(A).

wff(neg(A))       :-
  logical_atom(A).

wff(and(A, B))    :-
  wff(A),
  wff(B).

wff(or(A, B))     :-
  wff(A),
  wff(B).

wff(imp(A, B))    :-
  wff(A),
  wff(B).


% Task 2: cls(+F)
% cls(F) holds when the formula F is a clause; a clause is either a literal or
% a disjunction of literals, and a literal is either an atom or a negated atom.

% If formula A is not ground we want to fail without further backtracking.
cls(A)            :-
  \+ ground(A), !, fail.

cls(A)            :-
  logical_atom(A).

cls(neg(A))       :-
  logical_atom(A).

cls(or(A, B))     :-
  cls(A),
  cls(B).


% Task 3: ats(+F, -As)
% given the formula F, returns As as a duplicate-free list (in any order) of
% the atoms in F.

% If formula A is not ground we want to fail without further backtracking.
ats(F, As)                      :-
  ground(F),
  get_ats(F, As).

% If it is ground then we want to call createList, which recursively creates an initially empty accumulator and uses this to add each logical atom as a Head element to As
get_ats(F, As)                      :-
  createList(F, [], As), !.

createList(and(A, B), Acc, As)  :-
  createList(A, Acc, As2),
  createList(B, As2, As).

createList(or(A, B), Acc, As)   :-
  createList(A, Acc, As2),
  createList(B, As2, As).

createList(neg(A), Acc, As)     :-
  createList(A, Acc, As).

createList(imp(A, B), Acc, As)  :-
  createList(A, Acc, As2),
  createList(B, As2, As).

createList(A, Acc, As)          :-
  logical_atom(A),
  (\+ member(A, Acc) -> singleAtom([A|Acc], As);        % We only want to add the atom to the list if it is not already contained within the accumulator
  singleAtom(Acc, As)).

singleAtom(As, As).


% Task 4: t_value(+F, +Val, -V)
% Calculates the truth value V of the formula F, given the valuation Val.

t_value(F, Val, _) :-
  \+ ground(Val), !, fail;          % checks if Val is ground

  \+ (ats(F, ListOfAtomsInF),         % checks if F is a ground wff of logical atoms and stores a list of the atoms present in F
  atomsOf(Val, ListOfAtomsInF)), !, fail.

t_value(F, Val, V) :-
  ( t1_value(F, Val, V) ) -> assignTrue(V);         % if the truth value of F is true then let V be true; else false
  assignFalse(V).

% Recursively checks if Val ([Head|Tail]) only contains logical_atoms occuring in F
atomsOf([Head|Tail], ListOfAtomsInF) :-
  (logical_atom(Head), member(Head, ListOfAtomsInF)) -> atomsOf(Tail, ListOfAtomsInF).

atomsOf([], _).

% t1_value function computes the truth value of a formula F - it also checks if F is a wff
t1_value(A, Val, _)         :-
  member(A, Val).

t1_value(neg(A), Val, V)    :-
  \+ t1_value(A, Val, V).

t1_value(and(A,B), Val, _)  :-
  t1_value(A, Val, _V1),
  t1_value(B, Val, _V2).

t1_value(or(A,B), Val, _)   :-
  t1_value(A, Val, _V1);
  t1_value(B, Val, _V2).

t1_value(imp(A,B), Val, _)  :-
  \+ t1_value(A, Val, _V1);
  t1_value(B, Val, _V2).

assignTrue(t).
assignFalse(f).
