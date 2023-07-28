:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(clpb)).
:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdf_ntriples)).

% Loading data. Should be transfered to local
:- rdf_load("drugbank_small.nt").

% Load TMR Schema
:- rdf_load('https://raw.githubusercontent.com/veruskacz/CG-RDF/master/schema/model.ttl',
         [format('turtle'), register_namespaces(false),
         base_uri('http://anonymous.org/vocab/'), graph('http://anonymous.org/vocab')]).
:- rdf_load('https://raw.githubusercontent.com/veruskacz/CG-RDF/master/schema/model4I%203.0.ttl',
         [format('turtle'), register_namespaces(false),
         base_uri('http://anonymous.org/vocab4i/'), graph('http://anonymous.org/vocab4i')]).

% Load Guidelines' Data
:- rdf_load('https://raw.githubusercontent.com/veruskacz/CG-RDF/master/instance/CareAction%26DrugTypes.ttl',
         [format('turtle'), register_namespaces(false),
         base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/CareAction&DrugTypes')]).
:- rdf_load('https://raw.githubusercontent.com/veruskacz/CG-RDF/master/instance/Transition%26SituationTypes.ttl',
         [format('turtle'), register_namespaces(false),
         base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/Transition&SituationTypes')]).

:- rdf_load('https://raw.githubusercontent.com/veruskacz/CG-RDF/master/instance/CausationBeliefs-Nanopub.trig',
         [format('trig'), register_namespaces(false),
         base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/CausationBeliefs-Nanopub')]).

:- rdf_load('https://raw.githubusercontent.com/veruskacz/CG-RDF/master/instance/Reg%26Norms-Nanopub.trig',
         [format('trig'), register_namespaces(false),
         base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/Reg&Norms-Nanopub')]).
:- rdf_load('https://raw.githubusercontent.com/veruskacz/CG-RDF/master/instance/MergedReg%26Norms-Nanopub.trig',
         [format('trig'), register_namespaces(false),
         base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/MergedReg&Norms-Nanopub')]).

:- style_check(-discontiguous).

% Base model requires Guidelines, Recommendations and Transitions.
:- include(guidelines).
:- include(externalSources).
:- include(interactionRules).
:- include(interaction_graph).
:- include(recommendations).
:- include(transition).
:- include(preconditionnew).
:- use_rendering(graphviz).


%:- loadExternalBeliefs.
%:- inferInternalInteractions.
%:- inferExternalInteractions.
