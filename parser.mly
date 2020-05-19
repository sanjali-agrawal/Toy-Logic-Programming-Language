// This parser file is for parsing through the input file and outputs clauses. 

%{
  open Backend
%}
%token EOL
%token COMMA
%token LP
%token RP
%token IFF
%token <string> VAR
%token <string> CONS


%start main
%type <Backend.clause> main 				

%%
main : CLAUSE EOL{ $1 }
	;

CLAUSE : AM {F($1)}
	| AM IFF BODY {R($1,$3)}
	;

BODY : AM COMMA BODY {$1::$3}
	| AM {[$1]}
	;
AM:  CONS LP TL RP	{ (($1, List.length $3),$3)};
TL:	T COMMA TL	{$1::$3}
	| T	{[$1]}
	;
T: VAR {V($1)}
	| CONS {C($1)}
	| CONS LP TL RP {Node(($1, List.length $3),$3)}
	;
