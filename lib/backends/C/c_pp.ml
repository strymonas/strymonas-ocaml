(* Pretty-printer of C AST
 *)

open C_ast
open Format

let rec pr_typ : formatter -> typ -> unit = fun ppf -> function
  | Tvoid           -> pp_print_string ppf "void"
  | Tbool           -> pp_print_string ppf "bool" (* needs <stdbool.h> *)
  | Tchar           -> pp_print_string ppf "char"
  | Tshort          -> pp_print_string ppf "short"
  | Tint            -> pp_print_string ppf "int"
  | Tlong           -> pp_print_string ppf "long"
  | Tint64          -> pp_print_string ppf "int64_t"
  | Tuint           -> pp_print_string ppf "unsigned int"
  | Tulong          -> pp_print_string ppf "unsigned long"
  | Tfloat          -> pp_print_string ppf "float"
  | Tdouble         -> pp_print_string ppf "double"
  | Tnamed str      -> pp_print_string ppf str
  | Tptr ctp        -> fprintf ppf "%a *" pr_ctype ctp
  | Tarray (n,ctp)  -> fprintf ppf "%a [%d]" pr_ctype ctp n
 and pr_ctype : formatter -> ctype -> unit = fun ppf {specs;typ} ->
   fprintf ppf "%a%a" pr_specs specs pr_typ typ 
 and pr_specs : formatter -> spec list -> unit = fun ppf specs ->
     List.iter (fun e -> fprintf ppf "%a " pr_spec e) specs
 and pr_spec : formatter -> spec -> unit = fun ppf -> function
  | S_const         -> pp_print_string ppf "const"
  | S_volatile      -> pp_print_string ppf "volatile"
  | S_restrict      -> pp_print_string ppf "restrict"
  | S_static        -> pp_print_string ppf "static"
  | S_extern        -> pp_print_string ppf "extern"
  | S_inline        -> pp_print_string ppf "inline"

let pr_const : formatter -> constant -> unit = fun ppf -> function
  | Const_num str    -> pp_print_string ppf str
  | Const_char '\"'  -> pp_print_string ppf "'\"'"
  | Const_char c     -> fprintf ppf "'%s'" Char.(escaped c)
  | Const_string str -> fprintf ppf "\"%s\"" String.(escaped str)

let pr_unop : formatter -> unary_operator -> unit = fun ppf -> function
  | MINUS  -> pp_print_string ppf "-"
  | PLUS   -> pp_print_string ppf "+"
  | NOT    -> pp_print_string ppf "!"
  | BNOT   -> pp_print_string ppf "~"
  | MEMOF  -> pp_print_string ppf "*"
  | ADDROF -> pp_print_string ppf "&"

(* printed representations and precedences *)
let binops : (binary_operator * (string * int)) list = [
  (MUL, ("*", 12));
  (DIV, ("/", 12));
  (MOD, ("%", 12));
  (ADD, ("+", 11));
  (SUB, ("-", 11));
  (SHL, ("<<", 10));
  (SHR, (">>", 10));
  (LT, ("<", 9));
  (LE, ("<=", 9));
  (GT, (">", 9));
  (GE, (">=", 9));
  (EQ, ("==", 8));
  (NE, ("!=", 8));
  (BAND, ("&", 7));
  (XOR, ("^", 6));
  (BOR, ("|", 5));
  (AND, ("&&", 4));
  (OR, ("||", 3));
 ]

(* We don't do precedence pretty-printing for now *)
let pr_binop : formatter -> binary_operator -> unit = fun ppf op ->
  pp_print_string ppf (List.assoc op binops |> fst)


let binmodops : (binary_modifier * string) list = [
  (ASSIGN, "=");
  (ADD_ASSIGN, "+=");
  (SUB_ASSIGN, "-=");
  (MUL_ASSIGN, "*=");
  (DIV_ASSIGN, "/=");
  (MOD_ASSIGN, "%=");
  (BAND_ASSIGN, "&=");
  (BOR_ASSIGN, "|=");
  (XOR_ASSIGN, "^=");
  (SHL_ASSIGN, "<<=");
  (SHR_ASSIGN, ">>=");
 ]

let pr_assoc : ('a * string) list -> formatter -> 'a -> unit = fun lst ppf op ->
  pp_print_string ppf (List.assoc op lst)

(* Separators for pp_print_list *)
let pp_sep_comma : formatter -> unit -> unit = fun ppf () ->
  pp_print_string ppf ","; pp_print_cut ppf ()

let pp_sep_semi : formatter -> unit -> unit = fun ppf () ->
  pp_print_string ppf ";"; pp_print_space ppf ()

let rec pr_exp : formatter -> expression -> unit = fun ppf -> function
  | Nothing  -> ()
  | Const c  -> pr_const ppf c
  | Var v    -> pp_print_string ppf v
  | Unary (op,Var v) -> fprintf ppf "%a%s" pr_unop op v
  | Unary (op,exp)   -> fprintf ppf "%a(%a)" pr_unop op pr_exp exp
  | Label_addr l     -> fprintf ppf "&&%s" l
  | Binary (op,e1,e2) -> fprintf ppf "%a@ %a@ %a"
        pr_paren_exp e1 pr_binop op pr_paren_exp e2
  | Cond(e1,e2,e3)   -> fprintf ppf "(%a ? %a : %a)" 
        pr_exp e1 pr_exp e2 pr_exp e3
  | Cast (ctp,exp)   -> fprintf ppf "(%a)%a" pr_ctype ctp pr_paren_exp exp
  | Call(ef,args)    -> fprintf ppf "%a(%a)" pr_paren_exp ef
        (pp_print_list ~pp_sep:pp_sep_comma pr_exp) args
  | Comma ([],exp)   -> pr_exp ppf exp
  | Comma (sts,exp)  -> fprintf ppf "(%a,%a)"
        (pp_print_list ~pp_sep:pp_sep_comma pr_subexp) sts
        pr_exp exp
  | TYPE_SIZEOF ctp     -> fprintf ppf "sizeof(%a)" pr_ctype ctp
  | Index (e1,e2)       -> fprintf ppf "%a[%a]" pr_paren_exp e1 pr_exp e2
  | Memberof (e,fn)     -> fprintf ppf "%a.%s"  pr_paren_exp e fn
  | Memberofptr (e,fn)  -> fprintf ppf "%a->%s" pr_paren_exp e fn
 and pr_paren_exp : formatter -> expression -> unit = fun ppf -> function
   | Const _ | Var _ | Cond _ as e -> pr_exp ppf e
   | e -> fprintf ppf "(%a)" pr_exp e
 (* many statements like if (e) ... contain expressions in parens *)
 and pr_always_paren_exp : formatter -> expression -> unit = fun ppf e ->
   fprintf ppf "@[<2>(%a)@]" pr_exp e

 and pr_typedname : formatter -> typedname -> unit = fun ppf -> function
  | (v,{typ=Tarray(n,ct);specs}) ->
      fprintf ppf "%a%a %s[%d]" pr_specs specs pr_ctype ct v n
  | (v,ct) -> fprintf ppf "%a %s" pr_ctype ct v

 and pr_defn : formatter -> definition -> unit = fun ppf (tn,init) ->
  pr_typedname ppf tn;
  match init with
  | Init_none -> fprintf ppf ";"
  | Init_single e -> fprintf ppf "@[<2> =@ %a;@]" pr_exp e
  | Init_many es  -> fprintf ppf " = {@[<2>%a@]};"
        (pp_print_list ~pp_sep:pp_sep_comma pr_exp) es

(* Prints trailing semi-colon, except for blocks *)
 and pr_stmt : formatter -> statement -> unit = fun ppf -> function
  | NOP              -> fprintf ppf ";"
  | CALL _| UNMOD _ | BIMOD _ as e -> fprintf ppf "%a;" pr_subexp e
  | BLOCK b  -> pr_block ppf b
  | SEQUENCE stmts -> fprintf ppf "%a" (pp_print_list pr_stmt) stmts
  | IF(e,s1,NOP) -> fprintf ppf "if %a@,%a" pr_always_paren_exp e pr_substmt s1
  | IF(e,s1,s2) -> fprintf ppf "if %a@,%a@ else %a"
        pr_always_paren_exp e pr_substmt s1 pr_substmt s2
  | WHILE(e,stmt) -> fprintf ppf "while %a@,%a" 
        pr_always_paren_exp e pr_substmt stmt
  | DOWHILE(e,stmt) -> fprintf ppf "do@,%a@,while %a;"
        pr_substmt stmt pr_always_paren_exp e
  | FOR(sinit,e,sincr,stmt) -> fprintf ppf "for @[<2>(%a@ %a;@ %a)@]@,%a"
        pr_stmt sinit pr_exp e pr_subexp sincr pr_substmt stmt
  | BREAK          -> fprintf ppf "break;"
  | CONTINUE       -> fprintf ppf "continue;"
  | RETURN Nothing -> fprintf ppf "return;"
  | RETURN e -> fprintf ppf "return %a;" pr_paren_exp e
  | SWITCH(e,stmt) -> fprintf ppf "switch %a@,%a"
        pr_always_paren_exp e pr_substmt stmt
  | CASE(e,stmt)   -> fprintf ppf "case %a:@,%a"
        pr_exp e pr_substmt stmt
  | CASERANGE(e1,e2,stmt) -> fprintf ppf "case %a...%a:@,%a"
        pr_exp e1 pr_exp e2 pr_substmt stmt
  | DEFAULT stmt   -> fprintf ppf "default:@ %a;@," pr_substmt stmt
  | LABEL (l,stmt) -> fprintf ppf "%s:@ %a" l pr_substmt stmt
  | GOTO l         -> fprintf ppf "goto %s;" l
  | COMPGOTO e     -> fprintf ppf "goto *%a;" pr_paren_exp e
  (* the increment expression of a for-loop does not have a trailing
     semicolon 
   *)
 and pr_subexp : formatter -> statement -> unit = fun ppf -> function
  | CALL(ef,args)    -> fprintf ppf "%a@[<2>(%a)@]" pr_paren_exp ef
        (pp_print_list ~pp_sep:pp_sep_comma pr_exp) args
  | UNMOD(POSINCR,e) -> fprintf ppf "%a++" pr_paren_exp e
  | UNMOD(POSDECR,e) -> fprintf ppf "%a--" pr_paren_exp e
  | BIMOD (op,e1,e2) -> fprintf ppf "@[%a %a@ %a@]"
        pr_paren_exp e1 (pr_assoc binmodops) op pr_exp e2
  | _ -> assert false

  (* if as substamenents require special handling, due to their
     poor grammar
   *)
 and pr_substmt : formatter -> statement -> unit = fun ppf -> function
   | IF _ | SEQUENCE _ | DOWHILE _ as stmt ->
       fprintf ppf "{@[<v 2>%a@]}" pr_stmt stmt
   | BLOCK b -> pr_block ppf b
   | stmt -> fprintf ppf "@;<1 2>%a" pr_stmt stmt

 and pr_block : formatter -> block -> unit = fun ppf {blabels;bdefs;bstmts} ->
   let pr_labels ppf = function [] -> () | ls ->
     fprintf ppf "__label__ %a@,"
        (pp_print_list ~pp_sep:pp_sep_comma pp_print_string) ls
   in
   if blabels = [] && bdefs = [] then
     fprintf ppf "{@[<v 2>@,%a@]@,}"
     (pp_print_list pr_stmt) bstmts
   else
     fprintf ppf "{@[<v 2>@,%a%a@,%a@]@,}"
       pr_labels blabels
       (pp_print_list pr_defn) bdefs
       (pp_print_list pr_stmt) bstmts

let rec pr_decl : formatter -> declaration -> unit = fun ppf -> function
 | FUNDEF (ct,v,args,body) ->
     fprintf ppf "@,%a %s(%a)@,%a@,"
       pr_ctype ct v 
       (pp_print_list ~pp_sep:pp_sep_comma pr_typedname) args
       pr_block body
 | DECDEF defn -> pr_defn ppf defn
 (*
 | PROTO   of typedname * varname * typedname list  (* function prototype *)
 | TYPEDEF of typedname
 *)
 | _ -> failwith "other declarations not yet implemented"
