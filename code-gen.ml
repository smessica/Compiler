#use "semantic-analyser.ml";;
exception Wrong_input;;
exception X_sexpr of expr' ;;
(* This module is here for you convenience only!
   You are not required to use it.
   you are allowed to change it. *)
  module type CODE_GEN = sig
  (* This signature assumes the structure of the constants table is
     a list of key-value pairs:
     - The keys are constant values (Sexpr(x) or Void)
     - The values are pairs of:
       * the offset from the base const_table address in bytes; and
       * a string containing the byte representation (or a sequence of nasm macros)
         of the constant value
     For example: [(Sexpr(Nil), (1, "T_NIL"))]
   *)
  val make_consts_tbl : expr' list -> (constant * (int * string)) list

  (* This signature assumes the structure of the fvars table is
     a list of key-value pairs:
     - The keys are the fvar names as strings
     - The values are the offsets from the base fvars_table address in bytes
     For example: [("boolean?", 0)]
   *)  

  val make_fvars_tbl : expr' list -> (string * int) list

  (* If you change the types of the constants and fvars tables, you will have to update
     this signature to match: The first argument is the constants table type, the second 
     argument is the fvars table type, and the third is an expr' that has been annotated 
     by the semantic analyser.
   *)
  val generate : (constant * (int * string)) list -> (string * int) list -> expr' -> string
  end;;

module Code_Gen : CODE_GEN = struct
  
  let offset_counter init =
    let offset = ref init in
    let c n = 
      let current_offset = !offset in
      (offset := (!offset+n);current_offset) in c;;

  let make_consts_tbl asts =    
    let rec const_eq e1 e2 =
      match e1, e2 with
      | Void, Void -> true
      | (Sexpr s1),(Sexpr s2) -> sexpr_eq s1 s2
      | _ -> false in
      
    let rec remove_duplicates const_lst unique_list_ref =
      let rec list_mem lst expr = 
        match lst with
        | [] -> false
        | hd::tl -> if(const_eq hd expr) then true else (list_mem tl expr) in
      
      match const_lst with
      | [] -> ()
      | hd::tl -> (if(list_mem !unique_list_ref hd) then (remove_duplicates tl unique_list_ref) else ((unique_list_ref := !unique_list_ref @ [hd]); (remove_duplicates tl unique_list_ref))) in
    
    let rec const_extract const_lst_ref expr =
      let extract = const_extract const_lst_ref in
      let rec extract_const_sexpr sexpr =
        match sexpr with
        | Pair(hd, tl) -> (
          (extract_const_sexpr hd);
          (extract_const_sexpr tl);
          const_lst_ref := ((!const_lst_ref) @ [(Sexpr(sexpr))]))
        | Symbol(e) -> (const_lst_ref := (!const_lst_ref) @ [(Sexpr(String(e))); (Sexpr(Symbol(e)))])
        | e -> const_lst_ref := ((!const_lst_ref) @ [(Sexpr(e))]) in

      match expr with
      | Const'(Void) -> const_lst_ref := ((!const_lst_ref) @ [Void])
      | Const'(Sexpr(sexpr)) -> extract_const_sexpr sexpr
      | BoxSet'(v1,e1) -> extract e1 
      | If'(t1, th1, el1) -> (extract t1; extract th1; extract el1)
      | Seq'(l1) -> ignore (List.map extract l1);
      | Or'(l1) -> ignore (List.map extract l1);
      | Set'(var1, val1) -> (extract val1)
      | Def'(var1, val1) -> ( extract val1)
      | LambdaSimple'(vars1, body1) -> extract body1
      | LambdaOpt'(vars1, var1, body1) -> extract body1
      | Applic'(e1, args1) -> ignore (extract e1; (List.map extract args1));
      | ApplicTP'(e1, args1) -> ignore (extract e1; (List.map extract args1));
      | _ -> () in
    
    let rec create_tbl_entry offset_count tbl_entries_ref const_expr =
      let hd expr = 
        match expr with
        | (offset,addr)-> offset in

      match const_expr with
      | (Void) -> (const_expr, (offset_count(1), "MAKE_VOID"))
      | (Sexpr(Bool(false))) -> (const_expr, (offset_count(2), "MAKE_BOOL(0)")) 
      | (Sexpr(Bool(true))) -> (const_expr, (offset_count(2), "MAKE_BOOL(1)")) 
      | (Sexpr(Nil)) -> (const_expr, (offset_count(1), "MAKE_NIL"))
      | (Sexpr(Number(Float f1))) -> (const_expr,(offset_count(9), "MAKE_LITERAL_FLOAT("^(string_of_float f1)^")"))
      | (Sexpr(Number(Fraction (n1, d1)))) -> (const_expr,(offset_count(17), "MAKE_LITERAL_RATIONAL("^(string_of_int n1)^","^(string_of_int d1)^")"))
      | (Sexpr(Char(c1))) -> (const_expr, (offset_count(2), "MAKE_LITERAL_CHAR("^(string_of_int (Char.code c1))^")"))
      | (Sexpr(String(s1))) -> (
        let str_offset = 9 + (String.length s1) in 
        (const_expr, (offset_count(str_offset), "MAKE_LITERAL_STRING \""^(s1)^"\"")))
      | (Sexpr(Symbol(s1))) -> (
        let str_offset = (List.assoc (Sexpr(String(s1))) !tbl_entries_ref) in
        let str_offset = string_of_int (hd(str_offset)) in
        (const_expr, (offset_count(9), "MAKE_LITERAL_SYMBOL(const_tbl+"^str_offset^")")))
      | (Sexpr(Pair(car1, cdr1))) -> (
        let car_offset = (List.assoc (Sexpr(car1)) !tbl_entries_ref) in
        let car_offset = string_of_int (hd(car_offset)) in
        let cdr_offset = (List.assoc (Sexpr(cdr1)) !tbl_entries_ref) in
        let cdr_offset = string_of_int (hd(cdr_offset)) in
        (const_expr,(offset_count(17),"MAKE_LITERAL_PAIR(const_tbl+"^car_offset^",const_tbl+"^cdr_offset^")"))) in

    let offset_count = offset_counter 0 in
    let const_lst = ref [] in
    ignore (List.map (fun e -> (const_extract const_lst e)) asts);
    ignore (const_lst := [(Void); (Sexpr Nil); (Sexpr(Bool false)); (Sexpr(Bool true)); ] @ (!const_lst));
    let const_set = ref [] in
    ignore (remove_duplicates !const_lst const_set);
    let tbl_entries = ref [] in
    ignore (List.map (fun e-> (tbl_entries:= !tbl_entries @ [(create_tbl_entry offset_count tbl_entries e)])) !const_set);
    !tbl_entries ;;
  
  let make_fvars_tbl asts = 
    let prim_lst = ["boolean?"; "flonum?"; "rational?"; "pair?";
    "null?"; "char?"; "string?";"procedure?"; "symbol?"; "string-length";
    "string-ref"; "string-set!"; "make-string";"symbol->string"; "char->integer"; "integer->char";"exact->inexact"; "eq?";
    "+"; "*"; "/";"="; "<"; "numerator"; "denominator"; "gcd";
    ] in
    
    let rec extract_fvars fvars_lst_ref index_counter expr =
      let extract = extract_fvars fvars_lst_ref index_counter in

      let rec mem_lst elem lst =
        match lst with
        | [] -> false
        | hd::tl -> (
          match hd with
          |(var_name,idx)-> if (String.equal elem var_name) then true else (mem_lst elem tl)
        ) in
      
      match expr with
      | Var'(VarFree v1) -> 
      if ((mem_lst v1 !fvars_lst_ref) = false) then fvars_lst_ref:=!fvars_lst_ref@[(v1,index_counter(1))]
      | If'(t1, th1, el1) -> (extract t1; extract th1; extract el1)
      | Seq'(l1) -> ignore (List.map extract l1);
      | Or'(l1) -> ignore (List.map extract l1);
      | Set'(var1, val1) -> (extract val1)
      | Def'(var1, val1) -> ( extract val1)
      | LambdaSimple'(vars1, body1) -> extract body1
      | LambdaOpt'(vars1, var1, body1) -> extract body1
      | Applic'(e1, args1) -> ignore (extract e1; (List.map extract args1));
      | ApplicTP'(e1, args1) -> ignore (extract e1; (List.map extract args1));
      | _ -> () in


    let fvars_lst = ref [] in
    let index_counter = offset_counter 0 in
    ignore (List.map (fun prim -> fvars_lst := !fvars_lst @ [(prim,index_counter(1))]) prim_lst);
    ignore (List.map (extract_fvars fvars_lst index_counter) asts);
    !fvars_lst ;;
    
  let generate consts fvars e =   
    let get_offset const = 
      let hd e = 
        match e with
        | (offset,addr) -> offset in
      (hd (List.assoc const consts)) in
      
    match e with
    | Const'(const) -> let addr = string_of_int (get_offset const) in "mov rax,const_tbl+" ^ addr
    | _ -> raise (X_sexpr e);;

  end;;

