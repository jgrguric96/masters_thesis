:- style_check(-discontiguous).

% :- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/turtle)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_db)).

%:- read_ntriples("drugbank_sample.nt").
:- rdf_load("drugbank_small.nt").