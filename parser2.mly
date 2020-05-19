// This parser file parse through the queries we input in the console and outputs goals 

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
%type <Backend.goal> main

%%
main : AM EOL{ [$1] }
	| AM COMMA main { $1::$3 }
	;

AM:  CONS LP TL RP	{ (($1, List.length $3),$3)};
TL:	T COMMA TL	{$1::$3}
	| T	{[$1]}
	; 
T: VAR {V($1)}
	| CONS {C($1)}
	| CONS LP TL RP {Node(($1, List.length $3),$3)}
	;
