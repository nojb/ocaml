open Parser
open Asttypes
open Parsetree
open Longident
open Location

type strm =
  {
    lexer : Lexing.lexbuf -> token;
    lexbuf : Lexing.lexbuf;
    mutable lookahead : token Location.loc option;
  }

let make_loc start stop =
  { Location.loc_start = start; loc_end = stop; loc_ghost = false }

let merge_locs a b =
  {
    Location.loc_start = a.Location.loc_start;
    loc_end = b.Location.loc_end;
    loc_ghost = false;
  }

let mk_exp ?(attrs = []) ~loc desc =
  { pexp_desc = desc; pexp_loc = loc; pexp_loc_stack = []; pexp_attributes = attrs }

let mk_pat ?(attrs = []) ~loc desc =
  { ppat_desc = desc; ppat_loc = loc; ppat_loc_stack = []; ppat_attributes = attrs }

let reloc_exp ~loc exp =
  { exp with pexp_loc = loc; pexp_loc_stack = exp.pexp_loc :: exp.pexp_loc_stack }

let reloc_pat ~loc pat =
  { pat with ppat_loc = loc; ppat_loc_stack = pat.ppat_loc :: pat.ppat_loc_stack }

let mk_const ~loc desc = Ast_helper.Const.mk ~loc desc

let mk_ident lid loc = Location.mkloc lid loc

let current strm =
  match strm.lookahead with
  | Some tok -> tok
  | None ->
      let tok = strm.lexer strm.lexbuf in
      let loc_start = Lexing.lexeme_start_p strm.lexbuf in
      let loc_end = Lexing.lexeme_end_p strm.lexbuf in
      let located = Location.mkloc tok {loc_start; loc_end; loc_ghost = false} in
      strm.lookahead <- Some located;
      located

let next strm =
  match strm.lookahead with
  | Some _ -> strm.lookahead <- None
  | None -> ignore (current strm)

let peek strm = (current strm).Location.txt
let loc strm = (current strm).Location.loc

let expect strm expected =
  match peek strm with
  | tok when tok = expected ->
      let loc = loc strm in
      next strm;
      loc
  | _ ->
      raise (Syntaxerr.Error (Syntaxerr.Other (loc strm)))

let loc_of_expr e = e.pexp_loc
let loc_of_pat p = p.ppat_loc

let loc_union_expr a b = merge_locs (loc_of_expr a) (loc_of_expr b)
let loc_union_pat a b = merge_locs (loc_of_pat a) (loc_of_pat b)

let mk_bool ~loc b =
  let lid = if b then Lident "true" else Lident "false" in
  mk_exp ~loc (Pexp_construct (mk_ident lid loc, None))

let mk_unit ~loc =
  mk_exp ~loc (Pexp_construct (mk_ident (Lident "()") loc, None))

let mk_nil ~loc =
  mk_exp ~loc (Pexp_construct (mk_ident (Lident "[]") loc, None))

let mk_cons ~loc hd tl =
  let tuple =
    mk_exp ~loc (Pexp_tuple [ None, hd; None, tl ])
  in
  mk_exp ~loc
    (Pexp_construct (mk_ident (Lident "::") loc, Some tuple))

let mk_pat_nil ~loc =
  mk_pat ~loc (Ppat_construct (mk_ident (Lident "[]") loc, None))

let mk_pat_cons ~loc hd tl =
  let tuple = mk_pat ~loc (Ppat_tuple ([ None, hd; None, tl ], Closed)) in
  mk_pat ~loc
    (Ppat_construct (mk_ident (Lident "::") loc, Some ([], tuple)))

let rec list_expr_from loc items =
  match items with
  | [] -> mk_nil ~loc
  | hd :: tl ->
      let tail = list_expr_from loc tl in
      let loc = loc_union_expr hd tail in
      mk_cons ~loc hd tail

let rec list_pat_from loc items =
  match items with
  | [] -> mk_pat_nil ~loc
  | hd :: tl ->
      let tail = list_pat_from loc tl in
      let loc = loc_union_pat hd tail in
      mk_pat_cons ~loc hd tail

let unexpected strm =
  raise (Syntaxerr.Error (Syntaxerr.Other (loc strm)))

let rec parse_longident start_lid strm =
  match peek strm with
  | DOT ->
      next strm;
      begin match peek strm with
      | LIDENT s ->
          let sloc = loc strm in
          next strm;
          let lid = Ldot (start_lid, Location.mkloc s sloc) in
          parse_longident (Location.mkloc lid (make_loc start_lid.loc.loc_start sloc.loc_end)) strm
      | UIDENT s ->
          let sloc = loc strm in
          next strm;
          let lid = Ldot (start_lid, Location.mkloc s sloc) in
          parse_longident (Location.mkloc lid (make_loc start_lid.loc.loc_start sloc.loc_end)) strm
      | _ ->
          unexpected strm
      end
  | _ -> start_lid

let parse_lident strm =
  match peek strm with
  | LIDENT s ->
      let sloc = loc strm in
      next strm;
      let lid = parse_longident (Location.mkloc (Lident s) sloc) strm in
      mk_ident lid lid.loc
  | _ ->
      unexpected strm

let is_expr_start = function
  | LIDENT _ | UIDENT _ | INT _ | FLOAT _ | STRING _ | CHAR _
  | TRUE | FALSE | LPAREN | BEGIN | LBRACE | LBRACKET
  | LBRACKETBAR | FUN | FUNCTION | IF | WHILE | FOR
  | LET | MATCH | TRY | ASSERT | LAZY | PREFIXOP _
  | BANG | NEW | METAOCAML_BRACKET_OPEN | METAOCAML_ESCAPE -> true
  | _ -> false

let rec parse_pattern_main strm =
  parse_pattern_or strm

and parse_pattern_or strm =
  let first = parse_pattern_cons strm in
  let rec loop acc =
    match peek strm with
    | BAR ->
        next strm;
        let rhs = parse_pattern_cons strm in
        let loc = loc_union_pat acc rhs in
        let pat = mk_pat ~loc (Ppat_or (acc, rhs)) in
        loop pat
    | _ -> acc
  in
  loop first

and parse_pattern_cons strm =
  let left = parse_pattern_as strm in
  match peek strm with
  | COLONCOLON ->
      next strm;
      let right = parse_pattern_cons strm in
      let loc = merge_locs (loc_of_pat left) (loc_of_pat right) in
      mk_pat_cons ~loc left right
  | _ ->
      left

and parse_pattern_as strm =
  let pat = parse_pattern_simple strm in
  match peek strm with
  | AS ->
      next strm;
      begin match peek strm with
      | LIDENT s ->
          let sloc = loc strm in
          next strm;
          let loc = merge_locs (loc_of_pat pat) sloc in
          mk_pat ~loc (Ppat_alias (pat, mk_ident s sloc))
      | _ ->
          unexpected strm
      end
  | _ ->
      pat

and parse_pattern_simple strm =
  match peek strm with
  | UNDERSCORE ->
      let loc = loc strm in
      next strm;
      mk_pat ~loc Ppat_any
  | LIDENT s ->
      let loc = loc strm in
      next strm;
      mk_pat ~loc (Ppat_var (mk_ident s loc))
  | UIDENT name ->
      let loc = loc strm in
      next strm;
      let lid = parse_longident (Location.mkloc (Lident name) loc) strm in
      let loc = lid.loc in
      begin match peek strm with
      | LPAREN ->
          next strm;
          let arg = parse_pattern_main strm in
          let _ = expect strm RPAREN in
          mk_pat ~loc:(merge_locs loc (loc_of_pat arg))
            (Ppat_construct (mk_ident lid.txt loc, Some ([], arg)))
      | _ ->
          mk_pat ~loc (Ppat_construct (mk_ident lid.txt loc, None))
      end
  | INT (n, m) ->
      let loc = loc strm in
      next strm;
      mk_pat ~loc (Ppat_constant (mk_const ~loc (Pconst_integer (n, m))))
  | FLOAT (n, m) ->
      let loc = loc strm in
      next strm;
      mk_pat ~loc (Ppat_constant (mk_const ~loc (Pconst_float (n, m))))
  | CHAR c ->
      let loc = loc strm in
      next strm;
      mk_pat ~loc (Ppat_constant (mk_const ~loc (Pconst_char c)))
  | STRING (str, loc_str, delim) ->
      let loc = loc strm in
      next strm;
      mk_pat ~loc (Ppat_constant (mk_const ~loc (Pconst_string (str, loc_str, delim))))
  | TRUE ->
      let loc = loc strm in
      next strm;
      mk_pat ~loc (Ppat_construct (mk_ident (Lident "true") loc, None))
  | FALSE ->
      let loc = loc strm in
      next strm;
      mk_pat ~loc (Ppat_construct (mk_ident (Lident "false") loc, None))
  | LPAREN ->
      let s = (loc strm).loc_start in
      next strm;
      begin match peek strm with
      | RPAREN ->
          let {loc_end = stop; _} = loc strm in
          next strm;
          let loc = make_loc s stop in
          mk_pat ~loc (Ppat_construct (mk_ident (Lident "()") loc, None))
      | _ ->
          let first = parse_pattern_main strm in
          begin match peek strm with
          | COMMA ->
              next strm;
              let rec gather acc =
                match peek strm with
                | COMMA ->
                    next strm;
                    let p = parse_pattern_main strm in
                    gather (p :: acc)
                | RPAREN ->
                    next strm;
                    List.rev acc
                | _ ->
                    unexpected strm
              in
              let _ = expect strm RPAREN in
              let pats = first :: gather [] in
              let loc = merge_locs (loc_of_pat first) (loc_of_pat (List.hd (List.rev pats))) in
              mk_pat ~loc (Ppat_tuple (List.map (fun p -> (None, p)) pats, Closed))
          | RPAREN ->
              let {loc_end = stop; _} = loc strm in
              next strm;
              reloc_pat ~loc:(make_loc s stop) first
          | _ ->
              unexpected strm
          end
      end
  | LBRACKET ->
      let s = (loc strm).loc_start in
      next strm;
      begin match peek strm with
      | RBRACKET ->
          let {loc_end = stop; _} = loc strm in
          next strm;
          let loc = make_loc s stop in
          mk_pat_nil ~loc
      | _ ->
          let elems = parse_pattern_list strm RBRACKET in
          let loc =
            match List.rev elems with
            | [] -> make_loc s s
            | hd :: _ -> merge_locs (make_loc s s) (loc_of_pat hd)
          in
          list_pat_from loc elems
      end
  | LBRACKETBAR ->
      let {loc_start = s; _} = loc strm in
      next strm;
      begin match peek strm with
      | BARRBRACKET ->
          let {loc_end = stop; _} = loc strm in
          next strm;
          let loc = make_loc s stop in
          mk_pat ~loc (Ppat_array [])
      | _ ->
          let elems = parse_pattern_list strm BARRBRACKET in
          let loc = merge_locs (make_loc s s) (loc_of_pat (List.hd (List.rev elems))) in
          mk_pat ~loc (Ppat_array elems)
      end
  | _ ->
      unexpected strm

and parse_pattern_list strm closing =
  let rec gather acc =
    let elt = parse_pattern_main strm in
    match peek strm with
    | SEMI ->
        next strm;
        gather (elt :: acc)
    | tok when tok = closing ->
        next strm;
        List.rev (elt :: acc)
    | _ ->
        unexpected strm
  in
  gather []

and parse_function_params strm =
  let rec loop acc =
    match peek strm with
    | MINUSGREATER -> List.rev acc
    | _ ->
        let pat = parse_pattern_main strm in
        let loc = pat.ppat_loc in
        let param =
          { pparam_loc = loc;
            pparam_desc = Pparam_val (Nolabel, None, pat);
          }
        in
        loop (param :: acc)
  in
  loop []

and parse_cases strm =
  let rec case_loop acc =
    begin
      let pat = parse_pattern_main strm in
      let guard =
        match peek strm with
        | WHEN ->
            next strm;
            Some (parse_seq_expr strm)
        | _ -> None
      in
      let _ = expect strm MINUSGREATER in
      let rhs = parse_seq_expr strm in
      let case =
        { pc_lhs = pat; pc_guard = guard; pc_rhs = rhs }
      in
      match peek strm with
      | BAR ->
          next strm;
          case_loop (case :: acc)
      | _ -> List.rev (case :: acc)
    end
  in
  case_loop []

and parse_atomic_expr strm =
  match peek strm with
  | LIDENT s ->
      let loc = loc strm in
      next strm;
      let lid = parse_longident (Location.mkloc (Lident s) loc) strm in
      let loc = lid.loc in
      mk_exp ~loc (Pexp_ident (mk_ident lid.txt loc))
  | UIDENT s ->
      let loc = loc strm in
      next strm;
      let lid = parse_longident (Location.mkloc (Lident s) loc) strm in
      let loc = lid.loc in
      begin match peek strm with
      | LPAREN ->
          next strm;
          let arg = parse_seq_expr strm in
          let _ = expect strm RPAREN in
          let loc = merge_locs loc (loc_of_expr arg) in
          mk_exp ~loc (Pexp_construct (mk_ident lid.txt loc, Some arg))
      | _ ->
          mk_exp ~loc (Pexp_construct (mk_ident lid.txt loc, None))
      end
  | INT (n, m) ->
      let loc = loc strm in
      next strm;
      mk_exp ~loc (Pexp_constant (mk_const ~loc (Pconst_integer (n, m))))
  | FLOAT (f, m) ->
      let loc = loc strm in
      next strm;
      mk_exp ~loc (Pexp_constant (mk_const ~loc (Pconst_float (f, m))))
  | CHAR c ->
      let loc = loc strm in
      next strm;
      mk_exp ~loc (Pexp_constant (mk_const ~loc (Pconst_char c)))
  | STRING (str, loc_str, delim) ->
      let loc = loc strm in
      next strm;
      mk_exp ~loc (Pexp_constant (mk_const ~loc (Pconst_string (str, loc_str, delim))))
  | TRUE -> let loc = loc strm in next strm; mk_bool ~loc true
  | FALSE -> let loc = loc strm in next strm; mk_bool ~loc false
  | LPAREN ->
      let s = (loc strm).loc_start in
      next strm;
      begin match peek strm with
      | RPAREN ->
          let stop = (loc strm).loc_end in
          next strm;
          let loc = make_loc s stop in
          mk_unit ~loc
      | _ ->
          let first = parse_seq_expr strm in
          begin match peek strm with
          | COMMA ->
              let rec gather acc =
                match peek strm with
                | COMMA ->
                    next strm;
                    let e = parse_seq_expr strm in
                    gather (e :: acc)
                | RPAREN ->
                    next strm;
                    List.rev acc
                | _ ->
                    unexpected strm
              in
              let _ = expect strm RPAREN in
              let exprs = first :: gather [] in
              let last_loc = loc_of_expr (List.hd (List.rev exprs)) in
              let loc = merge_locs (loc_of_expr first) last_loc in
              mk_exp ~loc (Pexp_tuple (List.map (fun e -> (None, e)) exprs))
          | RPAREN ->
              let stop = (loc strm).loc_end in
              next strm;
              reloc_exp ~loc:(make_loc s stop) first
          | _ ->
              unexpected strm
          end
      end
  | BEGIN ->
      let s = (loc strm).loc_start in
      next strm;
      let body = parse_seq_expr strm in
      let _ = expect strm END in
      let loc = merge_locs (make_loc s s) (loc_of_expr body) in
      reloc_exp ~loc body
  | LBRACKET ->
      let s = (loc strm).loc_start in
      next strm;
      begin match peek strm with
      | RBRACKET ->
          let stop = (loc strm).loc_end in
          next strm;
          let loc = make_loc s stop in
          mk_nil ~loc
      | _ ->
          let elems = parse_expr_list strm RBRACKET in
          let loc =
            match List.rev elems with
            | [] -> make_loc s s
            | hd :: _ -> merge_locs (make_loc s s) (loc_of_expr hd)
          in
          list_expr_from loc elems
      end
  | LBRACKETBAR ->
      let s = (loc strm).loc_start in
      next strm;
      begin match peek strm with
      | BARRBRACKET ->
          let stop = (loc strm).loc_end in
          next strm;
          let loc = make_loc s stop in
          mk_exp ~loc (Pexp_array [])
      | _ ->
          let elems = parse_expr_list strm BARRBRACKET in
          let loc = merge_locs (make_loc s s) (loc_of_expr (List.hd (List.rev elems))) in
          mk_exp ~loc (Pexp_array elems)
      end
  | LBRACE ->
      let s = (loc strm).loc_start in
      next strm;
      let fields, base_loc =
        parse_record_expr_fields strm
      in
      let loc =
        match fields with
        | [] -> make_loc s s
        | (_, e) :: _ -> merge_locs (make_loc s s) (loc_of_expr e)
      in
      mk_exp ~loc (Pexp_record (List.rev fields, base_loc))
  | _ ->
      unexpected strm

and parse_expr_list strm closing =
  let rec gather acc =
    let elt = parse_seq_expr strm in
    match peek strm with
    | SEMI ->
        next strm;
        gather (elt :: acc)
    | tok when tok = closing ->
        next strm;
        List.rev (elt :: acc)
    | _ ->
        unexpected strm
  in
  gather []

and parse_record_expr_fields strm =
  let rec loop acc =
    match peek strm with
    | RBRACE ->
        next strm;
        acc, None
    | _ ->
        let lid = parse_lident strm in
        let _ = expect strm EQUAL in
        let expr = parse_seq_expr strm in
        let acc = (lid.txt, expr) :: acc in
        begin match peek strm with
        | SEMI ->
            next strm;
            loop acc
        | RBRACE ->
            next strm;
            acc, None
        | _ ->
            unexpected strm
        end
  in
  loop []

and parse_application strm =
  let rec gather func args =
    match peek strm with
    | tok when is_expr_start tok ->
        next strm;
        let arg = parse_prefix strm in
        gather func ((Nolabel, arg) :: args)
    | LABEL name ->
        next strm;
        let expr = parse_prefix strm in
        let args = (Labelled name, expr) :: args in
        gather func args
    | OPTLABEL name ->
        next strm;
        let expr = parse_prefix strm in
        gather func ((Optional name, expr) :: args)
    | _ ->
        func, List.rev args
  in
  let func = parse_atomic_expr strm in
  let func, args = gather func [] in
  if args = [] then func
  else
    let loc = loc_union_expr func (snd (List.hd (List.rev args))) in
    mk_exp ~loc (Pexp_apply (func, args))

and parse_prefix strm =
  match peek strm with
  | MINUS ->
      let loc = loc strm in
      next strm;
      let arg = parse_prefix strm in
      let loc = merge_locs loc (loc_of_expr arg) in
      let op = mk_exp ~loc (Pexp_ident (mk_ident (Lident "~-") loc)) in
      mk_exp ~loc (Pexp_apply (op, [ Nolabel, arg ]))
  | PLUS ->
      let loc = loc strm in
      next strm;
      let arg = parse_prefix strm in
      let loc = merge_locs loc (loc_of_expr arg) in
      let op = mk_exp ~loc (Pexp_ident (mk_ident (Lident "~+") loc)) in
      mk_exp ~loc (Pexp_apply (op, [ Nolabel, arg ]))
  | PREFIXOP name ->
      let loc = loc strm in
      next strm;
      let arg = parse_prefix strm in
      let loc = merge_locs loc (loc_of_expr arg) in
      let op = mk_exp ~loc (Pexp_ident (mk_ident (Lident ("~" ^ name)) loc)) in
      mk_exp ~loc (Pexp_apply (op, [ Nolabel, arg ]))
  | ASSERT ->
      let s = (loc strm).loc_start in
      next strm;
      let arg = parse_prefix strm in
      let loc = loc_union_expr arg arg in
      let loc = merge_locs (make_loc s s) loc in
      mk_exp ~loc (Pexp_assert arg)
  | LAZY ->
      let s = (loc strm).loc_start in
      next strm;
      let arg = parse_prefix strm in
      let loc = merge_locs (make_loc s s) (loc_of_expr arg) in
      mk_exp ~loc (Pexp_lazy arg)
  | _ ->
      parse_application strm

and infix_info = function
  | BARBAR -> Some (0, `Right, "||")
  | OR -> Some (0, `Right, "or")
  | AMPERAMPER -> Some (1, `Right, "&&")
  | AMPERSAND -> Some (1, `Right, "&")
  | INFIXOP0 s -> Some (2, `Left, s)
  | EQUAL -> Some (2, `Left, "=")
  | LESS -> Some (2, `Left, "<")
  | GREATER -> Some (2, `Left, ">")
  | HASHOP s -> Some (2, `Left, s)
  | INFIXOP1 s -> Some (3, `Right, s)
  | COLONCOLON -> Some (4, `Right, "::")
  | INFIXOP2 s -> Some (5, `Left, s)
  | PLUS -> Some (5, `Left, "+")
  | PLUSDOT -> Some (5, `Left, "+.")
  | MINUS -> Some (5, `Left, "-")
  | MINUSDOT -> Some (5, `Left, "-.")
  | PLUSEQ -> Some (5, `Left, "+=")
  | INFIXOP3 s -> Some (6, `Left, s)
  | STAR -> Some (6, `Left, "*")
  | PERCENT -> Some (6, `Left, "%")
  | INFIXOP4 s -> Some (7, `Right, s)
  | _ -> None

and parse_infix strm min_prec =
  let rec loop lhs =
    match peek strm with
    | tok ->
        begin match infix_info tok with
        | None -> lhs
        | Some (prec, assoc, name) ->
            if prec < min_prec then lhs
            else begin
              next strm;
              let next_min = match assoc with `Left -> prec + 1 | `Right -> prec in
              let rhs = parse_prefix strm |> parse_infix_tail next_min in
              let loc = loc_union_expr lhs rhs in
              let op_loc = loc in
              let op = mk_exp ~loc:op_loc (Pexp_ident (mk_ident (Lident name) op_loc)) in
              let app = mk_exp ~loc (Pexp_apply (op, [ Nolabel, lhs; Nolabel, rhs ])) in
              loop app
            end
        end
  and parse_infix_tail min_prec expr =
    match peek strm with
    | tok ->
        begin match infix_info tok with
        | None -> expr
        | Some (prec, _, _) when prec < min_prec -> expr
        | _ ->
            loop expr
        end
  in
  loop (parse_prefix strm)

and parse_if strm =
  let s = (expect strm IF).loc_start in
  let cond = parse_seq_expr strm in
  let _ = expect strm THEN in
  let thn = parse_seq_expr strm in
  let els =
    match peek strm with
    | ELSE ->
        next strm;
        Some (parse_seq_expr strm)
    | _ -> None
  in
  let loc =
    match els with
    | None -> merge_locs (make_loc s s) (loc_of_expr thn)
    | Some e -> merge_locs (make_loc s s) (loc_of_expr e)
  in
  mk_exp ~loc (Pexp_ifthenelse (cond, thn, els))

and parse_while strm =
  let s = (expect strm WHILE).loc_start in
  let cond = parse_seq_expr strm in
  let _ = expect strm DO in
  let body = parse_seq_expr strm in
  let _ = expect strm DONE in
  let loc = merge_locs (make_loc s s) (loc_of_expr body) in
  mk_exp ~loc (Pexp_while (cond, body))

and parse_for strm =
  let s = (expect strm FOR).loc_start in
  let pat = parse_pattern_main strm in
  let _ = expect strm EQUAL in
  let start = parse_seq_expr strm in
  let dir =
    match peek strm with
    | TO -> next strm; Upto
    | DOWNTO -> next strm; Downto
    | _ -> unexpected strm
  in
  let stop = parse_seq_expr strm in
  let _ = expect strm DO in
  let body = parse_seq_expr strm in
  let _ = expect strm DONE in
  let loc = merge_locs (make_loc s s) (loc_of_expr body) in
  mk_exp ~loc (Pexp_for (pat, start, stop, dir, body))

and parse_fun strm =
  let s = (expect strm FUN).loc_start in
  let params = parse_function_params strm in
  let _ = expect strm MINUSGREATER in
  let body = parse_seq_expr strm in
  let loc = merge_locs (make_loc s s) (loc_of_expr body) in
  mk_exp ~loc (Pexp_function (params, None, Pfunction_body body))

and parse_function_expr strm =
  let s = (expect strm FUNCTION).loc_start in
  begin match peek strm with
  | BAR -> next strm
  | _ -> ()
  end;
  let cases = parse_cases strm in
  let end_loc =
    match List.rev cases with
    | [] -> make_loc s s
    | hd :: _ -> loc_of_expr hd.pc_rhs
  in
  let loc = merge_locs (make_loc s s) end_loc in
  mk_exp ~loc (Pexp_function ([], None, Pfunction_cases (cases, loc, [])))

and parse_match strm =
  let _ = expect strm MATCH in
  let scrutinee = parse_seq_expr strm in
  ignore (expect strm WITH);
  begin match peek strm with
  | BAR -> next strm
  | _ -> ()
  end;
  let cases = parse_cases strm in
  let loc =
    match List.rev cases with
    | [] -> merge_locs (loc_of_expr scrutinee) (loc_of_expr scrutinee)
    | hd :: _ -> merge_locs (loc_of_expr scrutinee) (loc_of_expr hd.pc_rhs)
  in
  mk_exp ~loc (Pexp_match (scrutinee, cases))

and parse_try strm =
  let _ = expect strm TRY in
  let body = parse_seq_expr strm in
  let _ = expect strm WITH in
  begin match peek strm with
  | BAR -> next strm
  | _ -> ()
  end;
  let cases = parse_cases strm in
  let loc =
    match List.rev cases with
    | [] -> merge_locs (loc_of_expr body) (loc_of_expr body)
    | hd :: _ -> merge_locs (loc_of_expr body) (loc_of_expr hd.pc_rhs)
  in
  mk_exp ~loc (Pexp_try (body, cases))

and parse_let strm =
  let s = (expect strm LET).loc_start in
  let rec_flag =
    match peek strm with
    | REC -> next strm; Recursive
    | _ -> Nonrecursive
  in
  let bindings = parse_bindings strm in
  let _ = expect strm IN in
  let body = parse_seq_expr strm in
  let loc = merge_locs (make_loc s s) (loc_of_expr body) in
  mk_exp ~loc (Pexp_let (rec_flag, bindings, body))

and parse_bindings strm =
  let rec loop acc =
    let pat = parse_pattern_main strm in
    let _ = expect strm EQUAL in
    let expr = parse_seq_expr strm in
    let vb =
      Ast_helper.Vb.mk ~loc:(loc_union_pat pat pat) ~attrs:[] pat expr
    in
    match peek strm with
    | AND ->
        next strm;
        loop (vb :: acc)
    | _ -> List.rev (vb :: acc)
  in
  loop []

and parse_expr_no_seq strm =
  match peek strm with
  | LET -> parse_let strm
  | FUN -> parse_fun strm
  | FUNCTION -> parse_function_expr strm
  | IF -> parse_if strm
  | MATCH -> parse_match strm
  | TRY -> parse_try strm
  | WHILE -> parse_while strm
  | FOR -> parse_for strm
  | _ -> parse_infix strm 0

and parse_seq_expr strm =
  let rec loop () =
    let expr = parse_expr_no_seq strm in
    match peek strm with
    | SEMI ->
        next strm;
        begin match peek strm with
        | EOF | END | IN | RBRACE | RBRACKET | BARRBRACKET -> expr
        | _ ->
            let rhs = loop () in
            let loc = merge_locs (loc_of_expr expr) (loc_of_expr rhs) in
            mk_exp ~loc (Pexp_sequence (expr, rhs))
        end
    | _ ->
        expr
  in
  loop ()

let parse_expression lexer lexbuf =
  let strm = { lexer; lexbuf; lookahead = None } in
  let expr = parse_seq_expr strm in
  match peek strm with
  | EOF -> expr
  | _ -> unexpected strm

let parse_pattern lexer lexbuf =
  let strm = { lexer; lexbuf; lookahead = None } in
  let pat = parse_pattern_main strm in
  match peek strm with
  | EOF -> pat
  | _ -> unexpected strm
