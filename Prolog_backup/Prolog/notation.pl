% S1::p1 CMP S2::p2

:- op(700, xfx, <-).

expand(Var, _, _) :-
   var(Var), !, fail.
expand(Expr, rdf_value(S,P,E), E) :-
    compound(Expr),
    compound_name_arguments(Expr, '.', [S,Property]), !,
    property(Property, PrefLocal),
    rdf_global_id(PrefLocal, P).
expand(should, true, 0.5).
expand(Number, true, Number) :-
    number(Number).

is_compare(_==_).
is_compare(_>_).
is_compare(_>=_).
is_compare(_<_).

rdf_value(S,P,O) :-
    rdf(S,P,O0),
    (   value_of(O0, O1)
    ->  O = O1
    ).

value_of(literal(type('http://www.w3.org/2001/XMLSchema#string', should)), 0.5).
value_of(literal(type('http://www.w3.org/2001/XMLSchema#string', 'should-not')), -0.5).
value_of(V, V).

property(strength, vocab:strength).

goal_expansion(Compare0, (Ex1, Ex2, Compare)) :-
    is_compare(Compare0),
    Compare0 =.. [Op,V1,V2],
    expand(V1, Ex1, E1),
    expand(V2, Ex2, E2),
    Compare =.. [Op,E1,E2].
goal_expansion(V1 <- V2, (Ex2, V1 = E2)) :-
    expand(V2, Ex2, E2).
