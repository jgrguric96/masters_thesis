# Masters Thesis - Evaluating the TMR model using a Community-of-practice based methodology

This is the (cleaned) implementation of the one used in my thesis.

The only prerequisite to use this is to have prolog installed on your computer.

## How to use
1.  Open prolog
2.  Consult the file within the folder called TMR - start model
3.  Once fully loaded, experiments can be performed by first defining which guideline will be used, followed by what the user is interested in.

Examples: 
1.  rdf_global_id(data:'CIG-CKD-HT-AFib', Guideline), regulates(Guideline, Recommendation, Action, Strength, CausationBelief).
2.  rdf_global_id(data:'CIG-DU-TIA-Osteoporosis', Guideline), getExternallyInteractingRecommendations(Guideline, Recommendation, List).
