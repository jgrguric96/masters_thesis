% Based purely on following pre-defined files, it is missing some
% instances.
%

% Setup

:- style_check(-discontiguous).



:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf_http_plugin)).
%:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_db)).
%:- use_module(library(semweb/rdf11)).
%:- use_module(library(ugraphs), []).


% Load TMR Schema. Both the basic + trm4i

:- rdf_load('https://raw.githubusercontent.com/veruskacz/CG-RDF/master/schema/model.ttl', [format('turtle'), register_namespaces(false), base_uri('http://anonymous.org/vocab/'), graph('http://anonymous.org/vocab')]).

:- rdf_load('https://raw.githubusercontent.com/veruskacz/CG-RDF/master/schema/model4I%203.0.ttl',
         [format('turtle'), register_namespaces(false),
         base_uri('http://anonymous.org/vocab4i/'), graph('http://anonymous.org/vocab4i')]).


% Load Guidelines' Data. The defined instances.
% It seems to be missing - LODmapping
%
% Care Action

:- rdf_load('https://raw.githubusercontent.com/veruskacz/CG-RDF/master/instance/CareAction%26DrugTypes.ttl',
         [format('turtle'), register_namespaces(false),
         base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/CareAction&DrugTypes')]).

% Transition & Situation types

:- rdf_load('https://raw.githubusercontent.com/veruskacz/CG-RDF/master/instance/Transition%26SituationTypes.ttl',
         [format('turtle'), register_namespaces(false),
         base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/Transition&SituationTypes')]).

% Causation Beliefs - nanopub

:- rdf_load('https://raw.githubusercontent.com/veruskacz/CG-RDF/master/instance/CausationBeliefs-Nanopub.trig',
         [format('trig'), register_namespaces(false),
         base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/CausationBeliefs-Nanopub')]).

% Reg & Norms

:- rdf_load('https://raw.githubusercontent.com/veruskacz/CG-RDF/master/instance/Reg%26Norms-Nanopub.trig',
         [format('trig'), register_namespaces(false),
         base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/Reg&Norms-Nanopub')]).

% Merged reg & norms

:- rdf_load('https://raw.githubusercontent.com/veruskacz/CG-RDF/master/instance/MergedReg%26Norms-Nanopub.trig',
         [format('trig'), register_namespaces(false),
         base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/MergedReg&Norms-Nanopub')]).


% LOD - Useful later during import action hierarchy

:- rdf_load('https://raw.githubusercontent.com/veruskacz/CG-RDF/master/instance/LODmapping.ttl',
            [format('trig'), register_namespaces(false),
             base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/LODmapping')]) .



% Internal Interactions


%%%%%%%% doesnt work %%%
%
%:- include(guidelines).
%
%:- include(auxiliaryFunctions).
%:- include(nanopublication).
%
%
%:- include(preconditionnew).
%:- include(recommendations).
%:- include(transition).
%:- include(interactions).
%%%%%%%%%%%%%%%%%%%%%%%%%


%This one works but im redefining a lot of things
%:- consult(interactionRules).
%:- consult(interaction_graph).
%:- consult(externalSources).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%


:- include(interactionRules).
:- include(interaction_graph).
:- include(externalSources).
:- consult(guidelines).



%%%%% This one works, same problem as above
%:- ensure_loaded(guidelines).
%:- consult(externalSources).


% Calculating internal Interactions
% :- inferInternalInteractions.

% However, one can observe that the interactions calculated for the case study
% didn't consider the hierarchies (only a alternative interaction was
% found). This is because this information was not given. Instead on
% inserting it manually, we can import it from external datasets, such as
% drugbank (see in following section).

% :- include(externalSources).

% Load

:- use_module(library(semweb/rdf_ntriples)).
%:- rdf_load("drugbank_small.nt").
%:- rdf_load("drugbank_veruska.nq").
:- rdf_load("drugbank_veruska_small.nt").

:- propagGroupingCriteriaDrugToEventType.
:- drugbankAssertCausationFromCategory.
:- inferInternalInteractions.
