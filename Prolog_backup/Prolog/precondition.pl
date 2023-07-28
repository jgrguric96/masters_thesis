:- use_module(library(clpb)).
:- include(auxiliaryFunctions).
command(List) :-
findall([R1, R2, T, F, Mapping, F0, Mapping0],
(create_all_formulas(R1, R2, F, Mapping, _Tree1, _Tree2 ),
 copy_term(F, F0), copy_term(Mapping, Mapping0),
 (   sat(F)->  T=true ; T=false)
), List).

getNonSatRecommendationPair(R1, R2) :-
    create_all_formulas(R1, R2, F, _Mapping, _Tree1, _Tree2 ),
    \+ sat(F).

% given an URI, create the predicate logic formula that represents
% its precondition.
create_formula(URI, Formula, Mapping) :-
    build_tree(URI, Tree), !,
    assign_prolog_vars(Tree, Formula, Mapping).

% if the URI is of type PredicateType, we are dealing with a leaf
% and we don't need to recurse any more
build_tree(URI, p(Label)) :-
    rdf(URI, rdf:type, vocab:'PredicateType'),
    !,
    rdf(URI, rdfs:label, literal(Label)).
% if the URI has 'and' relationships, write a * in the tree and recurse
build_tree(URI, *(Parts)) :-
    rdf(URI, rdf:type, vocab:'PreconditionType'),
    rdf(URI, vocab:and, _),
    !,
    findall(Member, rdf(URI, vocab:and, Member), Members),
    maplist(build_tree, Members, Parts).
% if the URI has 'or' relationships, write a + in the tree and recurse
build_tree(URI, +(Parts)) :-
    rdf(URI, rdf:type, vocab:'PreconditionType'),
    rdf(URI, vocab:or, _),
    !,
    findall(Member, rdf(URI, vocab:or, Member), Members),
    maplist(build_tree, Members, Parts).
% if the URI has 'neg' relationships, write a ~ in the tree and recurse
build_tree(URI, ~(Part)) :-
    rdf(URI, rdf:type, vocab:'PreconditionType'),
    rdf(URI, vocab:neg, Member),
    !,
    build_tree(Member, Part).

% assign_prolog_vars takes the tree and
% creates a Prolog var for each variable in the formula
% recursive function, initial call
assign_prolog_vars(Tree, VarTree, Mapping) :-
    assign_prolog_vars(Tree, VarTree, [], Mapping).

% check if a Prolog var has already been created for this variable
% if so, move on. if not, add it to the mapping
assign_prolog_vars(p(Label), Var, Mapping0, Mapping) :-
    !,
    (   memberchk(Label=Var, Mapping0)
    ->  Mapping = Mapping0
    ;   Mapping = [Label=Var|Mapping0]
    ).

% get rid of the Boolean operation in front of the list and
% recurse down the tree
assign_prolog_vars(Tree, VarTree, Mapping0, Mapping) :-
    Tree =.. [Functor|Args0],
    !,
    foldl(assign_prolog_vars, Args0, Args, Mapping0, Mapping),
    VarTree =.. [Functor|Args].

% create_all_formulas(Rec1, Rec2, Formula, Mapping), copy_term(Formula, F1), sat(F1).
create_all_formulas(Rec1, Rec2, Formula, Mapping, Tree1, Tree2) :-
  % first get the global knowledge base, build its tree and
  % assign variables for later use
  rdf_global_id(data:'PCKnowledgeBase', KB),
  build_tree(KB, KBTree),
  !,
  rdf_reachable(IntTypeURI, rdfs:subClassOf,vocab4i:'InternalInteraction'),
  rdf(Interaction, rdf:type, IntTypeURI),
  findall(RecTemp, rdf(Interaction, vocab4i:relates, RecTemp), List),
  List = [Rec1, Rec2],
  rdf(Rec1, vocab:hasFilterSituation, PC1),
  rdf(Rec2, vocab:hasFilterSituation, PC2),
  findall(TreeTemp, build_tree(PC1, TreeTemp), [Tree1|_]),
  findall(TreeTemp, build_tree(PC2, TreeTemp), [Tree2|_]),
  assign_prolog_vars(*([Tree1, Tree2, KBTree]), Formula, Mapping).
