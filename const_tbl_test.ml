#use "code-gen.ml";;
(*
let rec is_proper_list sexpr = 
  match sexpr with
  | Pair(a,Nil) -> true
  | Pair(a,b) -> is_proper_list b
  | _-> raise X_syntax_error
  
  and pair_to_string sexpr = 
    let rec proper_list list = 
      match list with
      | Pair(a,Nil) -> sexpr_to_string_literal a
      | Pair(a,b) -> (sexpr_to_string_literal a)^" "^(proper_list b) in
      let rec improper_list list = 
        match list with
        | Pair(a,b) -> " "^(sexpr_to_string_literal a)^(proper_list b) 
        | _-> " . "^(sexpr_to_string_literal list) in
      if(is_proper_list sexpr) then "("^(proper_list sexpr)^")"
      else "("^(improper_list sexpr)^")"
  
  and sexpr_to_string_literal sexpr = 
    match sexpr with
  | Bool(b) -> string_of_bool b
  | Number(Fraction(n,d)) -> "("^ (string_of_int n)^" , "^ (string_of_int d)^")"
  | Number(Float(f)) -> string_of_float f
  | Nil -> "()"
  | Char(c) -> list_to_string [c]
  | String(s) -> s
  | Symbol(s) -> s
  | Pair(a,b) -> pair_to_string sexpr

  and sexpr_to_string sexpr =
    match sexpr with
    | Nil -> "()"
    | Bool(true) -> "#t"
    | Bool(false) -> "#f"
    | Char('\n') -> "#\\newline"		     
    | Char('\t') -> "#\\tab"
    | Char(' ') -> "#\\space" 
    | Char('\r') -> "#\\return"
    | Char('\012') -> "#\\page" 
    | Char(ch) -> Printf.sprintf "#\\%c" ch
    | Symbol(sym) -> sym
    | String(str) -> Printf.sprintf "\"%s\"" str
    | Number(Fraction(n,d)) -> "("^ (string_of_int n)^" , "^ (string_of_int d)^")"
    | Number(Float(x)) -> string_of_float(x)
    | Pair(car, cdr) ->
       sexpr_to_string_with_car (Printf.sprintf "(%s" (sexpr_to_string car)) cdr
  and sexpr_to_string_with_car car_str sexpr =
    match sexpr with
    | Nil -> Printf.sprintf "%s)" car_str
    | Pair(car, cdr) ->
       sexpr_to_string_with_car
         (Printf.sprintf "%s %s" car_str (sexpr_to_string car))
         cdr
    | sexpr -> Printf.sprintf "%s . %s)" car_str (sexpr_to_string sexpr);;

let rec print_const_table const_tbl = 
  let print_const_entry elem =
    match elem with 
    | (Sexpr(sexpr),(offset,addr)) ->"( "^(sexpr_to_string_literal sexpr) ^ " ,( " ^ (string_of_int(offset)) ^" , "^addr^" ))"
    | (Void,(offset,addr)) -> "( Void "^ " ,( " ^ (string_of_int(offset)) ^" , "^addr^" ))" in

  List.map (fun e -> Printf.printf "%s\n" (print_const_entry e)) const_tbl ;;
*)

(*let string_to_asts s = List.map Semantics.run_semantics
(Tag_Parser.tag_parse_expressions
   (Reader.read_sexprs s)) ;;
let asts = (string_to_asts "\"ab\"");;
let test = (Code_Gen.make_consts_tbl asts) ;;*)

let file_to_string f = 
  let ic = open_in f in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic; 
  (*ignore(Printf.printf "%s" s);*)
  s;;


let test_from_read s =  List.map Semantics.run_semantics (Tag_Parser.tag_parse_expressions (Reader.read_sexprs s));;
let test_make_consts_tbl s = (Code_Gen.make_consts_tbl (test_from_read s));;
let test_make_fvars_tbl s = (Code_Gen.make_fvars_tbl (test_from_read s));;
let test_generate s = List.map (Code_Gen.generate (test_make_consts_tbl s) (test_make_fvars_tbl s)) (test_from_read s);;

let s = file_to_string "foo.scm" ;;
let test1_0 = test_from_read s ;;
let test1_1 = test_make_consts_tbl s ;;
let test1_2 = test_make_fvars_tbl s ;;
let test1_3 = test_generate s ;;
