% produsul cartezian

cartesian([],_,[]).
cartesian([H1|T1], L2, R) :- aux(H1,L2,R1), cartesian(T1,L2,R2),!, append(R1,R2,R).

aux(_, [], []).
aux(X,[H|T],R) :- aux(X,T,R1), append([(X,H)],R1,R).

% reuniunea

contains(E,[E|_]).
contains(E,[_|R]) :- contains(E,R).

union([],X,X).
union([H|T],L2,R) :- union(T,L2,R), contains(H,L2).
union([H|T],L2,[H|R]) :- union(T,L2,R), \+ contains(H,L2).

% intersectia

intersection([],_,[]).
intersection([H|T],L2,R) :- intersection(T,L2,R), \+ contains(H,L2).
intersection([H|T],L2,[H|R]) :- intersection(T,L2,R), contains(H,L2).

% diferenta

diff([],_,[]).
diff([H|T],L2,[H|R]) :- diff(T,L2,R), \+ contains(H,L2), !.
diff([H|T],L2,R) :- diff(T,L2,R), contains(H,L2), !.

% power-set

pow([],[]).
pow([_|T],R) :- pow(T,R).
pow([H|T], [H|R]) :- pow(T,R).

% permutari

% Think of 'take(X,Z,W)' as being used in the "X put into W yields Z"
% sense here. Then the definitions could paraphrased as follows:

%	 Z is a permutation of [X|Y] provided W is a permutation of Y
%	 and then X is put into W to produce Z .
%        [] is the (only) permutation of [].

take(E,[E|R],R).
take(E,[F|R],[F|L]):- take(E,R,L).

perm([],[]).
perm([H|T],P) :- perm(T,P1), take(H,P,P1).

% aranjamente

take_k(0,_,[]).
take_k(K,[H|T],[H|R]) :- take_k(K1,T,R), K is K1+1.

ar(K,S,R) :- perm(S,R1), take_k(K,R1,R).

% combinari

comb(0,_,[]).
comb(N,[X|T],[X|Comb]):-N>0,N1 is N-1,comb(N1,T,Comb).
comb(N,[_|T],Comb):-N>0,comb(N,T,Comb).


inf(X) :- inf(X).
inf(_).

f(X) :- !, g(X).
f(a).
g(X) :- !, h(X).
g(b).
h(c).
h(d).

