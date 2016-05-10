contains(H,[H|_]).
contains(E,[H|T]) :- contains(E,T).

notcontains(_,[]).
notcontains(E,[H|T]) :- E \= H, notcontains(E,T).

%unique([],[]).
%unique([H|T],L) :- unique(T,L), contains(H,L).
%unique([H|T],[H|L]) :- unique(T,L), notcontains(H,L).

% really weird alternative, which deserves some reflection.
unique(L,Lp) :- aux_unique(L,[],Lp).
aux_unique(L,Vis,Lp) :- contains(E,L), notcontains(E,Vis), aux_unique(L,[E|Vis],Lp), !.
aux_unique(L,Vis,Vis).

% insertion sort
sort_ins(X,[],[X]).
sort_ins(X,[H|T], [H|R]) :- X > H, sort_ins(X,T,R).
sort_ins(X,[H|T], [X|[H|T]]) :- X =< H.

inssort([],[]).
inssort([H|T],R) :- inssort(T,Rp), sort_ins(H,Rp,R).

height(void,0).
height(node(L,_,R),N) :- height(L,N1), height(R,N2), N is max(N1,N2) + 1.

flatten(void,[]).
flatten(node(L,K,R),List) :- flatten(L,L1), flatten(R,L2), append(L1,[K|L2], List).

eval(val(X),C,X).
eval(plus(E1,E2),C,X) :- eval(E1,C,X1), eval(E2,C,X2), X is X1 + X2.
eval(mult(E1,E2),C,X) :- eval(E1,C,X1), eval(E2,C,X2), X is X1 * X2.
eval(var(X),C,R) :- contains([X,R],C).

% eval(mult(val(2),plus(var(x),var(y))),[[x,2],[y,3]],R).

% filter can only be implemented after discussing binding and freeing variables
%filterr(P,[],[]).
%filterr(P,[H|T],[H|R]) :- .
%filterr(P,[H|T],R) :- not(P(H)).

all(X,Y) :- !, fst(X), snd(Y).
all(x,y).
fst(a).
fst(b).
snd(a).
snd(b).

student(andrei).
student(marius).
lazy(marius).

p1(X):- student(X), \+ lazy(X).
p2(X):- \+ lazy(X), student(X).


