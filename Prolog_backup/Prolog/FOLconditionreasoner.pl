/*
 *
 *
 *
 *
 * 1. get interaction object with 2 recommendations
 * 2. get list of preconditions associated with them
 * 3. concatenate list, run sat
 * 4. return results
 * */


Statement :- True.

inverseTo(TransitionT1, TransitionT2) :-
	rdfs_individual_of(TransitionT1, vocab:'TransitionType'),
	rdfs_individual_of(TransitionT2, vocab:'TransitionType'),
    rdf(TransitionT1, vocab:'hasTransformableSituation', S1),
    rdf(TransitionT1, vocab:'hasExpectedSituation', S2),
    rdf(TransitionT2, vocab:'hasTransformableSituation', S2),
    rdf(TransitionT2, vocab:'hasExpectedSituation', S1),
    different(TransitionT1, TransitionT2).
