(* File lexer.mll *)
{
(* The type token is defined in parser.mli *)
open Parser
exception EOF
}
rule token = parse
    [' ' '\t' '\n']     { token lexbuf }     (* skip blanks *)
  | ['.' ]        { EOL }
  | [',' ]        { COMMA }
  | '('                                                              { LP                             }
  | ')'                                                              { RP                             }
  | ":-"                                                             { IFF                            }
  | ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_' ''']*    as ided          { VAR (ided)                      }
  | ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_' ''']*    as ided          { CONS (ided)                      }
  | eof {raise EOF}
