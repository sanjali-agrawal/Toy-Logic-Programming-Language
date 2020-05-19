
open Backend;;
open List;;


let db =                                                (* This is the clause list we use as a database to answer queries *)
          let filename = Sys.argv.(1) in 
          let file = open_in filename in                (* Opening the file inputted as an argument *)
          let lexbuf = Lexing.from_channel file in      (* Lexing file *)
          let rec loop temp=
              try
                  let result = Parser.main Lexer.token lexbuf in        (* now it is parsed by Parser and result is a clause *)
                  loop (append (temp)([result])) 
              with Lexer.EOF -> temp 
              
               
          in
          loop ([]);;

(*print_table(db);;*)

let _ = 
          Printf.printf "?-"; flush stdout;
          let rec loop () =
          try 
              let lexbuf = Lexing.from_channel stdin in                                             (* Query is lexed *)
                    while true do
                      let result = Parser2.main Lexer2.token lexbuf in                              (* now it is parsed by Parser2 and result is a goal *)
                      match query(result)(changec(db)(0))(false,[])(vars_p(result)) with            (* we call query function in the backend and if false, we print false, if true result is printed in the function itself *)
                      (false,one) -> (Printf.printf("false.\n");Printf.printf "\n?-"; flush stdout; ) 
                      | (true,one) ->(Printf.printf "\n?-";flush stdout;);

                    done
          with 
          Lexer2.EOF -> Printf.printf "\n\nEXITING\n\n"; flush stdout; exit 0                                   (* called in case EOF exception is raised *)
          | Stdlib.Parsing.Parse_error -> Printf.printf "ERROR :- Invalid syntax\n\n?-"; flush stdout; loop()   (* in case of parsing error this is raised *)
          | Failure(k) -> Printf.printf "ERROR :- Lexing: Empty token\n\n?-"; flush stdout; loop()              (* in case of lexing error this is raised *)
          in
          loop();;
            