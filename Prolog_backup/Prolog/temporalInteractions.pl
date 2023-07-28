%%% copied from server
% Temporal Prolog Rules

:- include(guidelines).
:- rdf_register_prefix(tempVocab, 'http://anonymous.org/vocab/temporalVocab/', [force = true]).
%:- rdf_register_prefix(vocab4i, 'http://anonymous.org/vocab4i/', [force = true]).

% 2nd attempt to assert deltas to Incompatibility Beliefs (next 3 functions)
% more efficient because it only calculates the delta if it was not yet asserted;
% a particular value for the delta can also be given, instead of calculated;
% the type of calculation can be specified as 'Specific' or 'General';
% can still be improved
assertAllDeltaMinSSSpec :-
    forall(rdf(SIB, rdf:type, tempVocab:'SpecificIncompatibilityBelief'),
           (assertDelta(SIB, 'DeltaSS', _, 'Specific') )
          ).

assertAllDeltaMinSSGen :-
    forall(rdf(SIB, rdf:type, tempVocab:'GeneralIncompatibilityBelief'),
           (assertDelta(SIB, 'DeltaSS', _, 'General') )
          ).

assertDelta(TempRelation, DeltaType, DeltaValue, CalcType) :-
		( %if there's alredy a delta of that type, then do not assert
            rdf_global_id(tempVocab:DeltaType, URIDeltaType),
            rdf(TempRelation, tempVocab:'hasTemporalConstraint', Delta),
            rdf(Delta, rdf:type, URIDeltaType) ->  true ;
            (   CalcType = 'Specific' -> calcDeltaMinSSSpec(TempRelation, DeltaValue)
                ;
		CalcType = 'General' -> calcDeltaMinSSGen(TempRelation, DeltaValue)
            ),

            %create an URI for the delta
            rdf_global_id(data:TR_ID, TempRelation),
	        concat_atom([DeltaType, TR_ID], NewID),
	    rdf_global_id(data:NewID, URIDelta),
	        concat_atom([DeltaType, 'Value', TR_ID], NewIDDeltaValue),
	    rdf_global_id(data:NewIDDeltaValue, URIDeltaValue),
            %assert new Delta to the TR with value and types
            rdf_global_id(tempVocab:DeltaType, URIDeltaType),
            rdf_assert(TempRelation, tempVocab:'hasTemporalConstraint', URIDelta, myEntailments),
            rdf_assert(URIDelta, rdf:type, URIDeltaType, myEntailments),
            rdf_assert(URIDelta, tempVocab:'hasUncertainTemporalExtent', URIDeltaValue, myEntailments),
            rdf_assert(URIDeltaValue, rdf:type, tempVocab:'TemporalExtent', myEntailments),
            rdf_assert(URIDeltaValue, tempVocab:'duration', literal(type(xsd:integer, DeltaValue)), myEntailments)
            ).

% Functions added to replicate the calculated deltas for incompatibility beliefs
% into the interactions that relates it with recommendations;
% The non-temporal interation will entail two temporal ones;
% Need to be improved for addressing interactions between complex recommendations
assertAllTemporalInteractions :-
    forall((rdf(Interaction, vocab4i:'relates', Belief),
            instanceOf(Belief, vocab:'IncompatibilityBelief'),
            rdf(Interaction, vocab4i:'relates', Rec1),
            rdf(Interaction, vocab4i:'relates', Rec2),
            Rec1 \= Rec2,
            instanceOf(Rec1, vocab:'ClinicalRecommendation'),
            instanceOf(Rec2, vocab:'ClinicalRecommendation'),
            rdf(Belief, tempVocab:'entails', TempBelief1),
            rdf(Belief, tempVocab:'entails', TempBelief2),
            TempBelief1 \= TempBelief2,
            instanceOf(TempBelief1, tempVocab:'TemporalIncompatibilityBelief'),
            instanceOf(TempBelief2, tempVocab:'TemporalIncompatibilityBelief'),
            rdf(TempBelief1, tempVocab:'hasTemporalConstraint', Delta1),
            rdf(TempBelief2, tempVocab:'hasTemporalConstraint', Delta2),
            rdf(Delta1, rdf:type, tempVocab:'DeltaSS'),
            rdf(Delta2, rdf:type, tempVocab:'DeltaSS')
           ),
           (assertTemporalInteraction(Interaction, Rec1, Rec2, Delta1),
			assertTemporalInteraction(Interaction, Rec2, Rec1, Delta2))
           ).

assertTemporalInteraction(Interaction, Subject, Object, Delta) :-
		(rdf(Interaction, tempVocab:'entails', TempInt),
             rdf(TempInt, rdf:type, tempVocab:'TemporalInteraction'),
             rdf(Subject, tempVocab:'subjectOf', TempInt),
             rdf(Object, tempVocab:'objectOf', TempInt)
            ->  true;
            %create an URI for the temporal interaction
            rdf_global_id(data:Inter_ID, Interaction),
            rdf_global_id(data:Sub_ID, Subject),
            rdf_global_id(data:Obj_ID, Object),
	        concat_atom(['Temp', Inter_ID, Sub_ID, Obj_ID], NewID),
	    rdf_global_id(data:NewID, URITempInt),
            %assert new temporal interaction
            rdf_assert(URITempInt, rdf:type, tempVocab:'TemporalInteraction'),
            rdf_assert(Interaction, tempVocab:'entails', URITempInt),
            rdf_assert(Subject, tempVocab:'subjectOf', URITempInt),
            rdf_assert(Object, tempVocab:'objectOf', URITempInt),
            rdf_assert(URITempInt, tempVocab:'hasTemporalConstraint', Delta)
		).

% Clean the dataset to re-run the experiments
/*
cleaningInteraction :-
    rdf_retractall(_, tempVocab:'entails', TempInt),
    rdf_retractall(TempInt, rdf:type, tempVocab:'TemporalInteraction'),
    rdf_retractall(_, tempVocab:'subjectOf', TempInt),
    rdf_retractall(_, tempVocab:'objectOf', TempInt).

cleaningBeliefs :-
    instanceOf(TempBelief1, tempVocab:'TemporalIncompatibilityBelief')
	rdf_retractall(TempBelief1, tempVocab:'hasTemporalConstraint', Delta),
    rdf_retractall(Delta, rdf:type, tempVocab:'DeltaSS'),
    rdf_retractall(Delta, tempVocab:'hasUncertainTemporalExtent', X).
*/

% This function returns the minimum delta start-start needed to avoid a specific
% incompatibility: the biggest expected temporal extent for the 'subject' event.
% It means whatever event are not meant to overlap with the subject-event should
% start after at least the maximum time expected for it
calcDeltaMinSSSpec(SIB, MinDelta) :-

            %% check if the received SIB parameter is indeed a SpecificIncompatibilityBelief
		rdf(SIB, rdf:type, tempVocab:'SpecificIncompatibilityBelief'),

            %% get the subject of the SIB
		rdf(EvT1, tempVocab:'subjectOf', SIB),

            %% get the Uncertain Temporal Extent (TEU), since its the maximum time between the events
		rdf(EvT1, tempVocab:'hasUncertainTemporalExtent', TEU),

            %% get the Atom and convert it to a number. Store the number in MinDelta
            rdf(TEU, tempVocab:'duration', literal(type(xsd:integer, Atom))),
			atom_number(Atom, MinDelta).

% This function returns the minimum delta start-start needed to avoid a general
% incompatibility: it assumes a general incompatibility between two source-events
% It entails a specific incompatibility between consequent-events caused by the formers.
% The latter are the ones that shouldn't overlap. A minimum delta is calculated for them.
% To this delta we sum up the biggest expected temporal extent for the 'subject'
% event plus the maximum delay expected for its consequent-event to start.
% Then we subtract the smallest expected temporal extent for the 'object'
% event as well as the minimum delay expected for its consequent event to start.
% It means whenever the consequent of two events are not meant to overlap
% The object-event should start after at least the calculated delta.
%% parameters:
%% GIB: General Incompatibility Belief to be calculated.
%% MinDelta: where the mimimal delta is going to be stored and returned.
calcDeltaMinSSGen(GIB, MinDelta) :-
            %% test if the GIB is actually an GeneralIncompatibilityBelief
		rdf(GIB, rdf:type, tempVocab:'GeneralIncompatibilityBelief'),

            %% get the associated Specific Incompatibility Belief
		rdf(GIB, tempVocab:'entails', SIB),

            %% get the subject General Event of the GIB and its uncertain duration.
		rdf(EvTGenSub, tempVocab:'subjectOf', GIB),
		rdf(EvTGenSub, tempVocab:'hasUncertainTemporalExtent', EvTGenSubUncertainTempExt),
            rdf(EvTGenSubUncertainTempExt, tempVocab:'duration', literal(type(xsd:integer, AtomUncertainDurGenSub))),

            %% get the object General Event of the GIB and its uncertain duration.
		rdf(EvTGenObj, tempVocab:'objectOf', GIB),
		rdf(EvTGenObj, tempVocab:'hasCertainTemporalExtent', EvTGenObjUncertainTempExt),
            rdf(EvTGenObjUncertainTempExt, tempVocab:'duration', literal(type(xsd:integer, AtomUncertainDurGenObj))),

            %% get the subject of the Specific Incompatibility Belief
		rdf(EvTSpecSub, tempVocab:'subjectOf', SIB), %Event Specific 1
            rdf(EvTSpecObj, tempVocab:'objectOf', SIB), % Event Specific 2
			calcDeltaMinSSSpec(SIB, SpecSSDelta), % SpecSSDelta is the minimal start-start delta between the specific events

		rdf(EvTGenSub, vocab:'causes', EvTSpecSub, CBSub:_), % Causation Belief 1
		rdf(CBSub, tempVocab:'hasTemporalConstraint', TempConstraintSub),
		rdf(TempConstraintSub, tempVocab:'hasUncertainTemporalExtent', CausationUTESub), % Temporal Extent Uncertain Temporal Constraint 1
            rdf(CausationUTESub, tempVocab:'duration', literal(type(xsd:integer, AtomUncertainDurCausationSub))),

		rdf(EvTGenObj, vocab:'causes', EvTSpecObj, CBObj:_),
		rdf(CBObj, tempVocab:'hasTemporalConstraint', TempConstraintObj),
		rdf(TempConstraintObj, tempVocab:'hasCertainTemporalExtent', CausationUTEObj),
            rdf(CausationUTEObj, tempVocab:'duration', literal(type(xsd:integer, AtomUncertainDurCausationObj))),

            %% get the values from the objects and calculate MinDelta.
			atom_number(AtomUncertainDurGenSub, UDGenSub),
			atom_number(AtomUncertainDurGenObj, UDGenObj),
			atom_number(AtomUncertainDurCausationSub, UDCausationSub),
			atom_number(AtomUncertainDurCausationObj, UDCausationObj),
		MinDelta is (UDGenSub-UDGenObj+SpecSSDelta+UDCausationSub-UDCausationObj).
