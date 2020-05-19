(* This is the Backend file that contains all the functions to carry out backtracking and unification *)

open List;;
open String;;
open Int;;

type variable = string;;
type symbol = string*int;;
type term = V of variable | C of string | Node of symbol * (term list);;
type predicate = symbol * (term list);;
type goal = predicate list;;
type clause = F of predicate | R of predicate * predicate list;;
type substitution = (variable * term) list;;

exception NOT_UNIFIABLE;;

(* Some helping function *)
let first a = match a with (x,y) -> x;;
let second a = match a with (x,y) -> y;;

(* This function compares two symbols just like comparing two strings *)
let compare (s1:symbol) (s2:symbol) :int =
  String.compare (first(s1)) (first(s2)) ;;

(* Compares two terms if they are same or not *)
let rec comparet (t1:term) (t2:term) :bool = match t1,t2 with
          V(x),V(y) -> if(String.compare(x)(y)=0) then true else false |
          C(c1), C(c2) -> if(String.compare(c1)(c2)=0) then true else false |
          Node(s,t),Node(a,b) -> if(compare(s)(a)=0) then for_all2(comparet)(t)(b) else false |
          _ -> false;;

(* The following two functions give head and body of some clause *)
let head (c:clause): predicate=match c with
          F(f) -> f |
          R(r,k) -> r;;
let body (c:clause): predicate list = match c with
          F(f) ->[] | R(r,l) -> l;;

(* Calculates height of a term (variable and constant height is taken 0) *)
let rec ht (t:term) : int =
  let max a b = if(a<b) then b else a                         (* gives max of two integers *)
  in
  match t with
  V(k) | C(k)-> 0 |                                                 (* Variable height is 0 *)    
  Node(s,l) -> 1 + fold_left (max) (0) (List.map (ht)(l));;   (* Height of a node is 1 + max of heights of all the other child nodes *)

(* Calculates size of a term (It gives a count of total number of terms in the given term) *)
let rec size (t:term) : int =
    match t with
    V(k) | C(k) -> 1 |                                              (* Variable size is 1 *)
    Node(s,l) -> fold_left (add) (1) (List.map (size)(l));;  (* Size of a node is 1 plus summation of sizes of all its child nodes *) 

(* Gives a list of all the variables in that term (list with no-duplicates)*)
let rec vars (t:term) : variable list =
    (* This function check takes two variable lists append them and outputs the appended list after removing duplicates using inbuit function sort_uniq*)
    let check (a:variable list) (b:variable list) = let c= append (a) (b) in sort_uniq(String.compare)(c)  
    in
    match t with
    V(k) -> k::[] |                                             (* list with variable k *)
    Node(s,l) -> fold_left (check) ([]) (List.map (vars)(l)) |
    C(k) -> [];;  

(* Gives list of variables in a goal *)
let rec vars_p (g:goal): variable list = 
      let va (p:predicate) = flatten(List.map(vars)(second(p))) in
      sort_uniq(String.compare)(flatten(List.map(va)(g)));;

(*This function simply applies substitution on a term *)
let rec subst (s:substitution) (t:term): term = match t with
    V(x) -> let temp = assoc_opt(x)(s) in if (temp=None) then V(x) else Option.get(temp)    (*It first checks if a variable is there in substitution to map to if not then it returns unchanged term(identity function result) *)
    | C(x) -> t
    | Node(sy,l) -> Node( sy , List.map (subst (s)) (l));;  

(* This function applies a substitution on a predicate *)
let rec subst_p (s:substitution) (p:predicate): predicate = 
          (first(p), List.map(subst(s))(second(p)));;
  
(* This function compares two strings and if they are equal outputs true and false otherwise *)
let comp s1 s2 = if (String.compare(s1)(s2)==0) then true else false;;          
    
(* The following function helps in printing the substitution and clauses *)

let rec print_t (tl:bool) (t:term)=match t with                                                       (* prints a term *)
    V(x) | C(x) -> Printf.printf "%s" x; if(tl) then Printf.printf(", ") else () |
    Node(sy,l) -> Printf.printf ("%s(")(first(sy)); (List.iter(print_t(true))(l)); Printf.printf(")");;

let print_f (f:predicate) = match f with                                                              (* prints a predicate *)
    (s,tl) -> Printf.printf("\n%s( \t")(first(s)); List.iter(print_t(true))(tl);;

let print_clause (c:clause) = match c with                                                            (* prints a clause *)
    F(f) -> print_f (f) ; Printf.printf "\n"|
    R(h,b) -> print_f(h); Printf.printf "iff\t"; List.iter(print_f)(b); Printf.printf "\n";;

let rec print_table (t:clause list) = match t with                                                    (* prints a clause list *)
          [] ->  ()|
          x::y -> print_clause(x); print_table(y);;

let rec print_subs (vl:variable list)(s:substitution) : unit =                                        (* prints a substitution *)
          let print (k:variable*term) = 
            Printf.printf("%s = ")(first(k));print_t(false)(second(k)); 
          in
          match List.length(vl) with
          0 ->() |
          1 -> print (hd(vl),assoc(hd(vl))(s)) |
          _ -> print (hd(vl),assoc(hd(vl))(s)); (Printf.printf ",\n" ); print_subs(tl(vl))(s);;

(* Composes two substitution *)
let rec composition (s1:substitution) (s2:substitution) : substitution =
                  let comp (v1:variable*term) (v2:variable*term) : int= match v1,v2 with           (* This function compares variables of tuples *) 
                                      (a,b),(c,d) -> String.compare(a)(c) 
                                in
                  let var (x:variable*term) :variable= match x with (v,t)->v in
                  let ter (x:variable*term) :term= match x with (v,t)->t in
                  let rec compose (s1:substitution) (s2:substitution) : substitution =match s1 with   (* This substitutes each of the term of s1 with composition *)
                              [] -> []|
                              x::xs -> (var x, subst(s2)(ter x)) ::compose(xs)(s2) in
                  sort_uniq(comp)(append(compose(s1)(s2))(s2));; 

(* outputs most general unifier of two terms *)
let rec mgut (t1:term) (t2:term) :substitution = 
                (* this function is called in case of unifying all the terms in case of a k-ary function symbols *)
                let rec loop (s:substitution) (a:term list) (b:term list) : substitution = match a, b with        
                            x::xs, y::ys -> loop (composition (s) (mgut (subst (s)(x))(subst (s)(y)))) (xs)(ys) |
                            [],[] -> s |
                              _ -> raise NOT_UNIFIABLE; 
                in
                match t1, t2 with 
                          C(x), C(y) -> if(String.compare(x)(y)==0) then [] else raise NOT_UNIFIABLE |
                          C(x), V(a) |V(a), C(x) -> [(a,C(x))] |
                          V(x),V(y) -> if(String.compare(x)(y)==0) then [] else [(y, V(x))] |                                
                          V(x),Node(s,l) | Node(s,l), V(x) -> if (mem(x)(vars(Node(s,l)))=true) then raise NOT_UNIFIABLE else [(x,Node(s,l))] |           (* Here if x is a member of variables of nodes then exception is raised *)
                          Node(s1,l1),Node(s2,l2) -> 
                                                  if(compare(s1)(s2)<>0) then raise NOT_UNIFIABLE else                                           (* If corresposnding symbols are different then exception is raised *)
                                                    loop ([]) (l1)(l2) |
                          _ -> raise NOT_UNIFIABLE;;   

(* applies a substitution on predicates and then unifies those predicates *)
let mgu (sub:substitution)(p1:predicate) (p2:predicate): substitution =
                      let rec loop (s:substitution)(a:term list) (b:term list) : substitution = 
                        match a, b with
                              x::xs, y::ys -> loop (composition (s) (mgut (subst(s)(x))(subst(s)(y)))) (xs)(ys) |
                              [],[] -> s |
                              _ -> raise NOT_UNIFIABLE; 
                      in
                      if(String.compare(first(first(p1)))(first(first(p2)))<>0) then raise NOT_UNIFIABLE 
                      else loop (sub) (second(p1))(second(p2));; 

(* In this function we change all the variables in the clause list so that they cannot be confused with the variables used in queries *)
let rec changec(cl:clause list) (i:int): clause list = 
                      let rec chn (i:int) (t:term):term = match t with
                                  V(x) -> V(x^"~"^string_of_int(i))               (* All the variables are marked with the clause number so that no two clause have same variables*)
                                  | C(c) -> t
                                  | Node(x,l) -> Node(x,List.map(chn(i))(l))
                      in
                      let chnge (i:int)(f:predicate) : predicate = first(f), List.map(chn(i))(second(f))
                      in
                      let chng (i:int)(c:clause) :clause = match c with
                                  F(f) -> F(chnge(i)(f)) |
                                  R(r,l) -> R(chnge(i)(r),List.map(chnge(i))(l)) 
                      in
                      match cl with
                        [] -> [] |
                        x::xs -> chng(i)(x)::changec(xs)(i+1)
;;

(* This function is called to ask if we have to search for other answers *)
let get_check (): bool = let check:string= read_line () in
                          if(String.compare(check)(";")==0) then true                       (* If input string is ; then get more answers *)
                          else if(String.compare(check)(".")==0 ) then false                (* If input string is . then we halt *)
                          else (Printf.printf("ERROR: Unknown Action");flush stdout; false);;     (* otherwise shows error *)

                          
(* This is the query called for each goal with respect to the clause list and answer is updated with each recursion and find all the variables given in the vars list *)

let rec query (gl :goal) (db: clause list) (answer:bool*substitution) (vars:variable list): (bool*substitution) =   
          let rec loop (bsub:bool*substitution)(gl:goal)(p1:predicate)(cl:clause list) =
                  match cl with
                        [] -> (false,[]) |
                        p2::xs ->
                        (try
                        let s=(mgu (second(bsub))(p1)(head(p2))) in                               (* It unifies the prediacte we unify it with the head of clause from clause list *)
                         match query  (List.append((body(p2)))((gl))) (db) (true,s) (vars) with                  (* If there is a unifier, predicate in the goal is replaced by subgoals i.e. the body
                                                                                                                    of the clause whose head is unified and we check for the rest goals by recursion *)
                                (true, one) -> (true, one) |                  (* If found we return true *)                          
                                (false, one) -> loop (bsub)(gl)(p1)(xs)       (* If the rest goals do not satisfy we check for the next clause in the clause list *)

                        with NOT_UNIFIABLE -> loop (bsub)(gl)(p1)(xs)  );                         (* If unifier doesnt exist this exception is raised and output is false *)
                  in      
          
          match gl with
              g::gs -> loop (answer)(gs)(g)(db) |                                   (* For each predicate in the goal, loop is called *)
              [] ->  match answer with                                              (* If there is no goal left we output according to the parameter answer which is being updated at every recusrion *)
                        (false, one) -> answer
                        | (true, []) -> Printf.printf("true.\n"); answer            (* we make sure that we do not print true unless all the goals are satisfied *)
                        | (true, x::xs) -> if(List.length(vars)==0) then (Printf.printf("true.\n"); answer) else    (* If there is any variable in vars list and there is a substitution needed for finding answer *)
                                           (print_subs(vars)(x::xs);                                                (* the substitution on the variables in the goal is printed *)
                                            let ch = get_check() in if(ch) then (false,[])                          (* then get_check is called to ask if more answers are needed, if yes, answer is changed to false *)
                                            else answer)                                                            (* and the function backtracks to last true answer by itself, else we simply return answer *)
;;


