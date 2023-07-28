% Based purely on following pre-defined files, it is missing some
% instances.
%

% Setup

:- style_check(-discontiguous).
:- set_prolog_stack(global, limit(100 000 000 000)).
:- set_prolog_stack(trail,  limit(20 000 000 000)).
:- set_prolog_stack(local,  limit(2 000 000 000)).

:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf_http_plugin)).
%:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_db)).
%:- use_module(library(semweb/rdf11)).
%:- use_module(library(ugraphs), []).


% Load TMR Schema. Both the basic + trm4i

:- rdf_load('schema/model.ttl', [format('turtle'), register_namespaces(false), base_uri('http://anonymous.org/vocab/'), graph('http://anonymous.org/vocab')]).

:- rdf_load('schema/model4I3.0.ttl',
         [format('turtle'), register_namespaces(false),
         base_uri('http://anonymous.org/vocab4i/'), graph('http://anonymous.org/vocab4i')]).


% Load Guidelines' Data. The defined instances.
% Care Action

:- rdf_load('instance/CareAction&DrugTypes.ttl',
         [format('turtle'), register_namespaces(false),
         base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/CareAction&DrugTypes')]).

% Transition & Situation types

:- rdf_load('instance/Transition&SituationTypes.ttl',
         [format('turtle'), register_namespaces(false),
         base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/Transition&SituationTypes')]).

% Causation Beliefs - nanopub

:- rdf_load('instance/CausationBeliefs-Nanopub.trig',
         [format('trig'), register_namespaces(false),
         base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/CausationBeliefs-Nanopub')]).

% Reg & Norms

:- rdf_load('instance/Reg&Norms-Nanopub.trig',
         [format('trig'), register_namespaces(false),
         base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/Reg&Norms-Nanopub')]).

% Merged reg & norms

:- rdf_load('instance/MergedReg&Norms-Nanopub.trig',
         [format('trig'), register_namespaces(false),
         base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/MergedReg&Norms-Nanopub')]).


% LOD - Useful later during import action hierarchy
% Will use altered one for now

:- rdf_load('instance/LODmapping.ttl',
            [format('trig'), register_namespaces(false),
             base_uri('http://anonymous.org/data/'), graph('http://anonymous.org/LODmapping')]) .

%:- rdf_load('instance/LODmapping_altered.ttl',
%            [format('trig'), register_namespaces(false),
%             base_uri('http://anonymous.org/data/'),
%             graph('http://anonymous.org/LODmapping')]) .




% Rest of the inclusions

:- include(interactionRules).
:- include(interaction_graph).
:- include(externalSources).
:- consult(guidelines).

% Load

:- use_module(library(semweb/rdf_ntriples)).
:- rdf_load("drugbank_veruska_small.nt").

:- propagGroupingCriteriaDrugToEventType.
:- drugbankAssertCausationFromCategory.
:- inferInternalInteractions.

% Load the rest of the datasets
:- rdf_load('sider_dump_new.nt').
:- rdf_load('dikb.ttl').
:- rdf_load('diag_mappings.nt').
:- rdf_load('drug_mappings.nt').
%:- rdf_load('dump-of-2012-generated-on-2012-07-09.nt'). % Memory leaks
%


% Testing this just in case
:- rdf_prefix(data, 'http://anonymous.org/data/').
:- rdf_prefix(vocab, 'http://anonymous.org/vocab/').
:- rdf_prefix(vocab4i, 'http://anonymous.org/vocab4i/').
:- rdf_prefix(oa, 'http,//www.w3.org/ns/oa#').
:- rdf_prefix(prov, 'http://www.w3.org/ns/prov#').
:- rdf_prefix(nanopub, 'http://www.nanopub.org/nschema#').

:- ensure_loaded(interactionRules).

% Finally we finish up with loading external beliefs
:- loadExternalBeliefs.
:- inferExternalInteractions.

% Sanity check on recommendations for CIG-OA-HT-DB
% :- rdf_global_id(data:'CIG-OA-HT-DB', Guideline),
%    getInternallyInteractingRecommendations(Guideline, IntType, List).
