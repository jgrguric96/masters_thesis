/** <application> �Hello, world!� GraphViz example

~~~sh
$ swipl -s hello.pl -g export -t halt
$ swipl -s hello.pl -g view -t halt
~~~

*/

:- use_module(library(yall)).

:- use_module(library(gv)).



%! export is det.

export(String,Name) :-
  gv_export('hello.svg', {String}/[Out0]>>format(Out0, String, [])).



%! view is det.

view :-
  example_(String),
  gv_view({String}/[Out0]>>format(Out0, String, [])).



% GENERICS %

example_("x [label=<Hello,<BR/>world!>,shape=diamond];\n").
