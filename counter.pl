:- use_module(library(clpfd)).

if_(C_1,Then_0,Else_0) -->
  { call(C_1,Truth) },
  { functor(Truth,_,0) },  % safety check
  (  { Truth == true }  -> phrase(Then_0)
  ;  { Truth == false },   phrase(Else_0)
 ).

if_(If_1, Then_0, Else_0) :-
 call(If_1, T),
 (  T == true -> call(Then_0)
 ;  T == false -> call(Else_0)
 ;  nonvar(T) -> throw(error(type_error(boolean,T),_))
 ;  /* var(T) */ throw(error(instantiation_error,_))
 ).

=(X, Y, T) :-
 (  X == Y -> T = true
 ;  X \= Y -> T = false
 ;  T = true, X = Y
 ;  T = false,
  dif(X, Y)                             % ISO extension
  % throw(error(instantiation_error,_)) % ISO strict
 ).

tcount(P_1,Xs,N) :-
 N #>= 0,
 list_pred_tcount_(Xs,P_1,0,N).

list_pred_tcount_([]    , _ ,N ,N).
list_pred_tcount_([X|Xs],P_1,N0,N) :-
 if_(call(P_1,X), (N1 is N0+1, N1 #=< N), N1 = N0),
 list_pred_tcount_(Xs,P_1,N1,N).
