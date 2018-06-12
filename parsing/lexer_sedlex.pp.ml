(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The lexer definition *)

(* open Lexing *)
open Misc
open Parser

let scurr lexbuf =
  let loc_start, loc_end = Sedlexing.lexing_positions lexbuf in
  {
  Location.loc_start;
  loc_end;
  loc_ghost = false
}

type error =
  | Illegal_character of char
  | Illegal_escape of string
  | Unterminated_comment of Location.t
  | Unterminated_string
  | Unterminated_string_in_comment of Location.t * Location.t
  | Keyword_as_label of string
  | Invalid_literal of string
  | Invalid_directive of string * string option

exception Error of error * Location.t

(* The table of keywords *)

let keyword_table =
  create_hashtable 149 [
    "and", AND;
    "as", AS;
    "assert", ASSERT;
    "begin", BEGIN;
    "class", CLASS;
    "constraint", CONSTRAINT;
    "do", DO;
    "done", DONE;
    "downto", DOWNTO;
    "else", ELSE;
    "end", END;
    "exception", EXCEPTION;
    "external", EXTERNAL;
    "false", FALSE;
    "for", FOR;
    "fun", FUN;
    "function", FUNCTION;
    "functor", FUNCTOR;
    "if", IF;
    "in", IN;
    "include", INCLUDE;
    "inherit", INHERIT;
    "initializer", INITIALIZER;
    "lazy", LAZY;
    "let", LET;
    "match", MATCH;
    "method", METHOD;
    "module", MODULE;
    "mutable", MUTABLE;
    "new", NEW;
    "nonrec", NONREC;
    "object", OBJECT;
    "of", OF;
    "open", OPEN;
    "or", OR;
    (*  "parser", PARSER; *)
    "private", PRIVATE;
    "rec", REC;
    "sig", SIG;
    "struct", STRUCT;
    "then", THEN;
    "to", TO;
    "true", TRUE;
    "try", TRY;
    "type", TYPE;
    "val", VAL;
    "virtual", VIRTUAL;
    "when", WHEN;
    "while", WHILE;
    "with", WITH;

    "lor", INFIXOP3("lor"); (* Should be INFIXOP2 *)
    "lxor", INFIXOP3("lxor"); (* Should be INFIXOP2 *)
    "mod", INFIXOP3("mod");
    "land", INFIXOP3("land");
    "lsl", INFIXOP4("lsl");
    "lsr", INFIXOP4("lsr");
    "asr", INFIXOP4("asr")
  ]

(* To buffer string literals *)

let string_buffer = Buffer.create 256
let reset_string_buffer () = Buffer.reset string_buffer
let get_stored_string () = Buffer.contents string_buffer

let store_string_char c = Buffer.add_char string_buffer c
let store_string_utf_8_uchar u = Buffer.add_utf_8_uchar string_buffer u
let store_string s = Buffer.add_string string_buffer s
let store_lexeme lexbuf = store_string (Sedlexing.Latin1.lexeme lexbuf)

(* To store the position of the beginning of a string and comment *)
let string_start_loc = ref Location.none;;
let comment_start_loc = ref [];;
let in_comment () = !comment_start_loc <> [];;
let is_in_string = ref false
let in_string () = !is_in_string
let print_warnings = ref true

(* Escaped chars are interpreted in strings unless they are in comments. *)
let store_escaped_char lexbuf c =
  if in_comment () then store_lexeme lexbuf else store_string_char c

let store_escaped_uchar lexbuf u =
  if in_comment () then store_lexeme lexbuf else store_string_utf_8_uchar u

let with_comment_buffer comment lexbuf =
  let start_loc = scurr lexbuf  in
  comment_start_loc := [start_loc];
  reset_string_buffer ();
  let end_loc = comment lexbuf in
  let s = get_stored_string () in
  reset_string_buffer ();
  let loc = { start_loc with Location.loc_end = end_loc.Location.loc_end } in
  s, loc

(* To translate escape sequences *)

let hex_digit_value d = (* assert (d in '0'..'9' 'a'..'f' 'A'..'F') *)
  let d = Char.code d in
  if d >= 97 then d - 87 else
  if d >= 65 then d - 55 else
    d - 48

let hex_num_value lexbuf ~first ~last =
  let rec loop acc i = match i > last with
    | true -> acc
    | false ->
        let value = hex_digit_value (Sedlexing.Latin1.lexeme_char lexbuf i) in
        loop (16 * acc + value) (i + 1)
  in
  loop 0 first

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let char_for_decimal_code lexbuf i =
  let c = 100 * (Char.code(Sedlexing.Latin1.lexeme_char lexbuf i) - 48) +
          10 * (Char.code(Sedlexing.Latin1.lexeme_char lexbuf (i+1)) - 48) +
          (Char.code(Sedlexing.Latin1.lexeme_char lexbuf (i+2)) - 48) in
  if (c < 0 || c > 255) then
    if in_comment ()
    then 'x'
    else raise (Error(Illegal_escape (Sedlexing.Latin1.lexeme lexbuf),
                      scurr lexbuf))
  else Char.chr c

let char_for_octal_code lexbuf i =
  let c = 64 * (Char.code(Sedlexing.Latin1.lexeme_char lexbuf i) - 48) +
          8 * (Char.code(Sedlexing.Latin1.lexeme_char lexbuf (i+1)) - 48) +
          (Char.code(Sedlexing.Latin1.lexeme_char lexbuf (i+2)) - 48) in
  Char.chr c

let char_for_hexadecimal_code lexbuf i =
  let byte = hex_num_value lexbuf ~first:i ~last:(i+1) in
  Char.chr byte

let uchar_for_uchar_escape lexbuf =
  let err e =
    raise
      (Error (Illegal_escape (Sedlexing.Latin1.lexeme lexbuf ^ e), scurr lexbuf))
  in
  let len = Sedlexing.lexeme_end lexbuf - Sedlexing.lexeme_start lexbuf in
  let first = 3 (* skip opening \u{ *) in
  let last = len - 2 (* skip closing } *) in
  let digit_count = last - first + 1 in
  match digit_count > 6 with
  | true -> err ", too many digits, expected 1 to 6 hexadecimal digits"
  | false ->
      let cp = hex_num_value lexbuf ~first ~last in
      if Uchar.is_valid cp then Uchar.unsafe_of_int cp else
        err (", " ^ Printf.sprintf "%X" cp ^ " is not a Unicode scalar value")

(* recover the name from a LABEL or OPTLABEL token *)

let get_label_name lexbuf =
  let s = Sedlexing.Latin1.lexeme lexbuf in
  let name = String.sub s 1 (String.length s - 2) in
  if Hashtbl.mem keyword_table name then
    raise (Error(Keyword_as_label name, scurr lexbuf));
  name

(* Update the current location with file name and line number. *)

let update_loc _lexbuf _file _line _absolute _chars =
  (* let pos = lexbuf.lex_curr_p in
   * let new_file = match file with
   *   | None -> pos.pos_fname
   *   | Some s -> s
   * in
   * Sedlexing.set_position lexbuf { pos with
   *                        pos_fname = new_file;
   *                        pos_lnum = if absolute then line else pos.pos_lnum + line;
   *                        pos_bol = pos.pos_cnum - chars;
   *                      } *)
  ()

let preprocessor = ref None

let escaped_newlines = ref false

(* Warn about Latin-1 characters used in idents *)

let warn_latin1 lexbuf =
  Location.deprecated (scurr lexbuf)"ISO-Latin1 characters in identifiers"

let handle_docstrings = ref true
let comment_list = ref []

let add_comment com =
  comment_list := com :: !comment_list

let add_docstring_comment ds =
  let com =
    ("*" ^ Docstrings.docstring_body ds, Docstrings.docstring_loc ds)
  in
  add_comment com

let comments () = List.rev !comment_list

(* Error report *)

open Format

let report_error ppf = function
  | Illegal_character c ->
      fprintf ppf "Illegal character (%s)" (Char.escaped c)
  | Illegal_escape s ->
      fprintf ppf "Illegal backslash escape in string or character (%s)" s
  | Unterminated_comment _ ->
      fprintf ppf "Comment not terminated"
  | Unterminated_string ->
      fprintf ppf "String literal not terminated"
  | Unterminated_string_in_comment (_, loc) ->
      fprintf ppf "This comment contains an unterminated string literal@.\
                   %aString literal begins here"
        Location.print_error loc
  | Keyword_as_label kwd ->
      fprintf ppf "`%s' is a keyword, it cannot be used as label name" kwd
  | Invalid_literal s ->
      fprintf ppf "Invalid literal %s" s
  | Invalid_directive (dir, explanation) ->
      fprintf ppf "Invalid lexer directive %S" dir;
      begin match explanation with
      | None -> ()
      | Some expl -> fprintf ppf ": %s" expl
      end

let () =
  Location.register_error_of_exn
    (function
      | Error (err, loc) ->
          Some (Location.error_of_printer loc report_error err)
      | _ ->
          None
    )

let newline = [%sedlex.regexp? Star '\013', '\010']
let blank = [%sedlex.regexp? ' ' | '\009' | '\012']
let lowercase = [%sedlex.regexp? 'a'..'z' | '_']
let uppercase = [%sedlex.regexp? 'A'..'Z']
let identchar = [%sedlex.regexp? 'A'..'Z' | 'a'..'z' | '_' | '\'' | '0'..'9']
let lowercase_latin1 = [%sedlex.regexp? 'a'..'z' | '\223'..'\246' | '\248'..'\255' | '_']
let uppercase_latin1 = [%sedlex.regexp? 'A'..'Z' | '\192'..'\214' | '\216'..'\222']
let identchar_latin1 =
  [%sedlex.regexp? 'A'..'Z' | 'a'..'z' | '_' | '\192'..'\214' | '\216'..'\246' | '\248'..'\255' | '\'' | '0'..'9']
let symbolchar =
  [%sedlex.regexp? Chars "!$%&*+-./:<=>?@^|~"]
let dotsymbolchar =
  [%sedlex.regexp? Chars "!$%&*+-/:=>?@^|~"]
let decimal_literal =
  [%sedlex.regexp? '0'..'9', Star ('0'..'9' | '_')]
let hex_digit =
  [%sedlex.regexp? '0'..'9' | 'A'..'F' | 'a'..'f']
let hex_literal =
  [%sedlex.regexp? '0', ('x' | 'X'), ('0'..'9' | 'A'..'F' | 'a'..'f'), Star ('0'..'9' | 'A'..'F' | 'a'..'f' | '_')]
let oct_literal =
  [%sedlex.regexp? '0', ('o' | 'O'), ('0'..'7'), Star ('0'..'7' | '_')]
let bin_literal =
  [%sedlex.regexp? '0', ('b' | 'B'), ('0'..'1'), Star ('0'..'1' | '_')]
let int_literal =
  [%sedlex.regexp? decimal_literal | hex_literal | oct_literal | bin_literal]
let float_literal =
  [%sedlex.regexp? ('0'..'9'), Star ('0'..'9' | '_'),
                 Opt ('.', Star ('0'..'9' | '_')),
                 Opt (('e' | 'E'), Opt ('+' | '-'), ('0'..'9'), Star ('0'..'9' | '_'))]
let hex_float_literal =
  [%sedlex.regexp? '0', ('x' | 'X'),
                 ('0'..'9' | 'A'..'F' | 'a'..'f'), Star ('0'..'9' | 'A'..'F' | 'a'..'f' | '_'),
                 Opt ('.', Star ('0'..'9' | 'A'..'F' | 'a'..'f' | '_')),
                 Opt (('p' | 'P'), Opt ('+' | '-'), ('0'..'9'), Star ('0'..'9' | '_'))]
let literal_modifier = [%sedlex.regexp? 'G'..'Z' | 'g'..'z']

let rec token lexbuf =
  match%sedlex lexbuf with
  | '\\', newline ->
      if not !escaped_newlines then
        raise (Error(Illegal_character (Sedlexing.Latin1.lexeme_char lexbuf 0),
                     scurr lexbuf));
      update_loc lexbuf None 1 false 0;
      token lexbuf
  | newline ->
      update_loc lexbuf None 1 false 0;
      EOL
  | Plus blank ->
      token lexbuf
  | '_' ->
      UNDERSCORE
  | '~' ->
      TILDE
  | '~', lowercase, Star identchar, ':' ->
      LABEL (get_label_name lexbuf)
  | '~', lowercase_latin1, Star identchar_latin1, ':' ->
      warn_latin1 lexbuf; LABEL (get_label_name lexbuf)
  | '?' ->
      QUESTION
  | '?', lowercase, Star identchar, ':' ->
      OPTLABEL (get_label_name lexbuf)
  | '?', lowercase_latin1, Star identchar_latin1, ':' ->
      warn_latin1 lexbuf; OPTLABEL (get_label_name lexbuf)
  | lowercase, Star identchar ->
      begin let s = Sedlexing.Latin1.lexeme lexbuf in
        try Hashtbl.find keyword_table s
        with Not_found -> LIDENT s
      end
  | lowercase_latin1, Star identchar_latin1 ->
      warn_latin1 lexbuf; LIDENT (Sedlexing.Latin1.lexeme lexbuf)
  | uppercase, Star identchar ->
      UIDENT(Sedlexing.Latin1.lexeme lexbuf)       (* No capitalized keywords *)
  | uppercase_latin1, Star identchar_latin1 ->
      warn_latin1 lexbuf; UIDENT(Sedlexing.Latin1.lexeme lexbuf)
  | int_literal -> INT (Sedlexing.Latin1.lexeme lexbuf, None)
  | (int_literal (* as lit *)), (literal_modifier (* as modif *)) ->
      (* FIXME *)
      let len = Sedlexing.lexeme_length lexbuf in
      let lit = Sedlexing.Latin1.sub_lexeme lexbuf 0 (len - 1) in
      let modif = Sedlexing.Latin1.lexeme_char lexbuf (len - 1) in
      (* END FIXME *)
      INT (lit, Some modif)
  | float_literal | hex_float_literal ->
      FLOAT (Sedlexing.Latin1.lexeme lexbuf, None)
  | ((float_literal | hex_float_literal) (* as lit *)), (literal_modifier (* as modif *)) ->
      (* FIXME *)
      let len = Sedlexing.lexeme_length lexbuf in
      let lit = Sedlexing.Latin1.sub_lexeme lexbuf 0 (len - 1) in
      let modif = Sedlexing.Latin1.lexeme_char lexbuf (len - 1) in
      (* END FIXME *)
      FLOAT (lit, Some modif)
  | (float_literal | hex_float_literal | int_literal), Plus identchar ->
      raise (Error(Invalid_literal (Sedlexing.Latin1.lexeme lexbuf),
                   scurr lexbuf))
  | '\"' ->
      reset_string_buffer();
      is_in_string := true;
      let string_start = lexbuf.lex_start_p in
      string_start_loc := scurr lexbuf;
      string lexbuf;
      is_in_string := false;
      lexbuf.lex_start_p <- string_start;
      STRING (get_stored_string(), None)
  | '{', Star lowercase, '|' ->
      reset_string_buffer();
      let delim = Sedlexing.Latin1.lexeme lexbuf in
      let delim = String.sub delim 1 (String.length delim - 2) in
      is_in_string := true;
      let string_start = lexbuf.lex_start_p in
      string_start_loc := scurr lexbuf;
      quoted_string delim lexbuf;
      is_in_string := false;
      lexbuf.lex_start_p <- string_start;
      STRING (get_stored_string(), Some delim)
  | '\'', newline, '\'' ->
      update_loc lexbuf None 1 false 1;
      CHAR (Sedlexing.Latin1.lexeme_char lexbuf 1)
  | '\'', Compl ('\\' | '\'' | '\010' | '\013'), '\'' ->
      CHAR(Sedlexing.Latin1.lexeme_char lexbuf 1)
  | "\'\\", ('\\' | '\'' | '\"' | 'n' | 't' | 'b' | 'r' | ' '), "\'" ->
      CHAR(char_for_backslash (Sedlexing.Latin1.lexeme_char lexbuf 2))
  | "\'\\", ('0'..'9'), ('0'..'9'), ('0'..'9'), "\'" ->
      CHAR(char_for_decimal_code lexbuf 2)
  | "\'\\", 'o', ('0'..'3'), ('0'..'7'), ('0'..'7'), "\'" ->
      CHAR(char_for_octal_code lexbuf 3)
  | "\'\\", 'x', ('0'..'9' | 'a'..'f' | 'A'..'F'), ('0'..'9' | 'a'..'f' | 'A'..'F'), "\'" ->
      CHAR(char_for_hexadecimal_code lexbuf 3)
  | "\'\\", any ->
      let l = Sedlexing.Latin1.lexeme lexbuf in
      let esc = String.sub l 1 (String.length l - 1) in
      raise (Error(Illegal_escape esc, scurr lexbuf))
  | "(*" ->
      let s, loc = with_comment_buffer comment lexbuf in
      COMMENT (s, loc)
  | "(**" ->
      let s, loc = with_comment_buffer comment lexbuf in
      if !handle_docstrings then
        DOCSTRING (Docstrings.docstring s loc)
      else
        COMMENT ("*" ^ s, loc)
  | "(**", (Plus '*' (* as stars *)) ->
      (* FIXME *)
      let len = Sedlexing.lexeme_length lexbuf in
      let stars = Sedlexing.Latin1.sub_lexeme lexbuf 3 (len - 3) in
      (* END FIXME *)
      let s, loc =
        with_comment_buffer
          (fun lexbuf ->
             store_string ("*" ^ stars);
             comment lexbuf)
          lexbuf
      in
      COMMENT (s, loc)
  | "(*)" ->
      if !print_warnings then
        Location.prerr_warning (scurr lexbuf) Warnings.Comment_start;
      let s, loc = with_comment_buffer comment lexbuf in
      COMMENT (s, loc)
  | "(*", (Star '*' (* as stars *)), "*)" ->
      (* FIXME *)
      let len = Sedlexing.lexeme_length lexbuf in
      let stars = Sedlexing.Latin1.sub_lexeme lexbuf 2 (len - 4) in
      (* END FIXME *)
      if !handle_docstrings && stars="" then
        (* (**) is an empty docstring *)
        DOCSTRING(Docstrings.docstring "" (scurr lexbuf))
      else
        COMMENT (stars, scurr lexbuf)
  | "*)" ->
      let loc = scurr lexbuf in
      Location.prerr_warning loc Warnings.Comment_not_end;
      lexbuf.Sedlexing.Latin1.lex_curr_pos <- lexbuf.Sedlexing.Latin1.lex_curr_pos - 1;
      let curpos = lexbuf.lex_curr_p in
      lexbuf.lex_curr_p <- { curpos with pos_cnum = curpos.pos_cnum - 1 };
      STAR
  | "#" ->
      begin
        let at_beginning_of_line pos = (pos.pos_cnum = pos.pos_bol) in
        if not (at_beginning_of_line lexbuf.lex_start_p)
        then HASH
        else try directive lexbuf with Failure _ -> HASH
      end
  | "&"  -> AMPERSAND
  | "&&" -> AMPERAMPER
  | "`"  -> BACKQUOTE
  | "\'" -> QUOTE
  | "("  -> LPAREN
  | ")"  -> RPAREN
  | "*"  -> STAR
  | ","  -> COMMA
  | "->" -> MINUSGREATER
  | "."  -> DOT
  | ".." -> DOTDOT
  | ".", dotsymbolchar, (Star symbolchar (* as s *)) ->
      (* FIXME *)
      let len = Sedlexing.lexeme_length lexbuf in
      let s = Sedlexing.Latin1.sub_lexeme lexbuf 2 (len - 2) in
      (* END FIXME *)
      DOTOP s
  | ":"  -> COLON
  | "::" -> COLONCOLON
  | ":=" -> COLONEQUAL
  | ":>" -> COLONGREATER
  | ";"  -> SEMI
  | ";;" -> SEMISEMI
  | "<"  -> LESS
  | "<-" -> LESSMINUS
  | "="  -> EQUAL
  | "["  -> LBRACKET
  | "[|" -> LBRACKETBAR
  | "[<" -> LBRACKETLESS
  | "[>" -> LBRACKETGREATER
  | "]"  -> RBRACKET
  | "{"  -> LBRACE
  | "{<" -> LBRACELESS
  | "|"  -> BAR
  | "||" -> BARBAR
  | "|]" -> BARRBRACKET
  | ">"  -> GREATER
  | ">]" -> GREATERRBRACKET
  | "}"  -> RBRACE
  | ">}" -> GREATERRBRACE
  | "[@" -> LBRACKETAT
  | "[@@"  -> LBRACKETATAT
  | "[@@@" -> LBRACKETATATAT
  | "[%"   -> LBRACKETPERCENT
  | "[%%"  -> LBRACKETPERCENTPERCENT
  | "!"  -> BANG
  | "!=" -> INFIXOP0 "!="
  | "+"  -> PLUS
  | "+." -> PLUSDOT
  | "+=" -> PLUSEQ
  | "-"  -> MINUS
  | "-." -> MINUSDOT

  | "!", Plus symbolchar ->
      PREFIXOP(Sedlexing.Latin1.lexeme lexbuf)
  | ('~' | '?'), Plus symbolchar ->
      PREFIXOP(Sedlexing.Latin1.lexeme lexbuf)
  | ('=' | '<' | '>' | '|' | '&' | '$'), Star symbolchar ->
      INFIXOP0(Sedlexing.Latin1.lexeme lexbuf)
  | ('@' | '^'), Star symbolchar ->
      INFIXOP1(Sedlexing.Latin1.lexeme lexbuf)
  | ('+' | '-'), Star symbolchar ->
      INFIXOP2(Sedlexing.Latin1.lexeme lexbuf)
  | "**", Star symbolchar ->
      INFIXOP4(Sedlexing.Latin1.lexeme lexbuf)
  | '%'     -> PERCENT
  | ('*' | '/' | '%'), Star symbolchar ->
      INFIXOP3(Sedlexing.Latin1.lexeme lexbuf)
  | '#', Plus (symbolchar | '#') ->
      HASHOP(Sedlexing.Latin1.lexeme lexbuf)
  | eof -> EOF
  | _ ->
      raise (Error(Illegal_character (Sedlexing.Latin1.lexeme_char lexbuf 0),
                   scurr lexbuf))

(* and directive lexbuf =
 *   match%sedlex lexbuf with
 *   | (Star (' ' | '\t'), (Plus ('0'..'9') as num), Star (' ' | '\t'),
 *      ("\"", (Star (Compl ('\010' | '\013' | '\"')) as name), "\"") as directive),
 *     Star (Compl ('\010' | '\013')) ->
 *       begin match int_of_string num with
 *       | exception _ ->
 *           (\* PR#7165 *\)
 *           let loc = Location.curr lexbuf in
 *           let explanation = "line number out of range" in
 *           let error = Invalid_directive ("#" ^ directive, Some explanation) in
 *           raise (Error (error, loc))
 *       | line_num ->
 *           (\* Documentation says that the line number should be
 *              positive, but we have never guarded against this and it
 *              might have useful hackish uses. *\)
 *           update_loc lexbuf (Some name) (line_num - 1) true 0;
 *           token lexbuf
 *       end *)

and comment lexbuf =
  match%sedlex lexbuf with
    "(*" ->
      comment_start_loc := (scurr lexbuf) :: !comment_start_loc;
      store_lexeme lexbuf;
      comment lexbuf
  | "*)" ->
      begin match !comment_start_loc with
      | [] -> assert false
      | [_] -> comment_start_loc := []; scurr lexbuf
      | _ :: l -> comment_start_loc := l;
          store_lexeme lexbuf;
          comment lexbuf
      end
  | "\"" ->
      string_start_loc := scurr lexbuf;
      store_string_char '\"';
      is_in_string := true;
      begin try string lexbuf
      with Error (Unterminated_string, str_start) ->
      match !comment_start_loc with
      | [] -> assert false
      | loc :: _ ->
          let start = List.hd (List.rev !comment_start_loc) in
          comment_start_loc := [];
          raise (Error (Unterminated_string_in_comment (start, str_start),
                        loc))
      end;
      is_in_string := false;
      store_string_char '\"';
      comment lexbuf
  | "{", Star lowercase, "|" ->
      let delim = Sedlexing.Latin1.lexeme lexbuf in
      let delim = String.sub delim 1 (String.length delim - 2) in
      string_start_loc := scurr lexbuf;
      store_lexeme lexbuf;
      is_in_string := true;
      begin try quoted_string delim lexbuf
      with Error (Unterminated_string, str_start) ->
      match !comment_start_loc with
      | [] -> assert false
      | loc :: _ ->
          let start = List.hd (List.rev !comment_start_loc) in
          comment_start_loc := [];
          raise (Error (Unterminated_string_in_comment (start, str_start),
                        loc))
      end;
      is_in_string := false;
      store_string_char '|';
      store_string delim;
      store_string_char '}';
      comment lexbuf

  | "\'\'" ->
      store_lexeme lexbuf; comment lexbuf
  | "\'", newline, "\'" ->
      update_loc lexbuf None 1 false 1;
      store_lexeme lexbuf;
      comment lexbuf
  | "\'", Compl ('\\' | '\'' | '\010' | '\013'), "\'" ->
      store_lexeme lexbuf; comment lexbuf
  | "\'\\", ('\\' | '\"' | '\'' | 'n' | 't' | 'b' | 'r' | ' '), "\'" ->
      store_lexeme lexbuf; comment lexbuf
  | "\'\\", ('0'..'9'), ('0'..'9'), ('0'..'9'), "\'" ->
      store_lexeme lexbuf; comment lexbuf
  | "\'\\", 'x', ('0'..'9' | 'a'..'f' | 'A'..'F'), ('0'..'9' | 'a'..'f' | 'A'..'F'), "\'" ->
      store_lexeme lexbuf; comment lexbuf
  | eof ->
      begin match !comment_start_loc with
      | [] -> assert false
      | loc :: _ ->
          let start = List.hd (List.rev !comment_start_loc) in
          comment_start_loc := [];
          raise (Error (Unterminated_comment start, loc))
      end
  | newline ->
      update_loc lexbuf None 1 false 0;
      store_lexeme lexbuf;
      comment lexbuf
  | _ ->
      store_lexeme lexbuf; comment lexbuf

and string lexbuf =
  match%sedlex lexbuf with
    '\"' ->
      ()
  (* | '\\', newline, (Star (' ' | '\t') as space) ->
   *     update_loc lexbuf None 1 false (String.length space);
   *     if in_comment () then store_lexeme lexbuf;
   *     string lexbuf *)
  | '\\', ('\\' | '\'' | '\"' | 'n' | 't' | 'b' | 'r' | ' ') ->
      store_escaped_char lexbuf
        (char_for_backslash(Sedlexing.Latin1.lexeme_char lexbuf 1));
      string lexbuf
  | '\\', ('0'..'9'), ('0'..'9'), ('0'..'9') ->
      store_escaped_char lexbuf (char_for_decimal_code lexbuf 1);
      string lexbuf
  | '\\', 'o', ('0'..'3'), ('0'..'7'), ('0'..'7') ->
      store_escaped_char lexbuf (char_for_octal_code lexbuf 2);
      string lexbuf
  | '\\', 'x', ('0'..'9' | 'a'..'f' | 'A'..'F'), ('0'..'9' | 'a'..'f' | 'A'..'F') ->
      store_escaped_char lexbuf (char_for_hexadecimal_code lexbuf 2);
      string lexbuf
  | '\\', 'u', '{', Plus hex_digit, '}' ->
      store_escaped_uchar lexbuf (uchar_for_uchar_escape lexbuf);
      string lexbuf
  | '\\', any ->
      if not (in_comment ()) then begin
        (*  Should be an error, but we are very lax.
                  raise (Error (Illegal_escape (Sedlexing.Latin1.lexeme lexbuf),
                                Location.curr lexbuf))
        *)
        let loc = scurr lexbuf in
        Location.prerr_warning loc Warnings.Illegal_backslash;
      end;
      store_lexeme lexbuf;
      string lexbuf
  | newline ->
      if not (in_comment ()) then
        Location.prerr_warning (scurr lexbuf) Warnings.Eol_in_string;
      update_loc lexbuf None 1 false 0;
      store_lexeme lexbuf;
      string lexbuf
  | eof ->
      is_in_string := false;
      raise (Error (Unterminated_string, !string_start_loc))
  | _ ->
      store_string_char(Sedlexing.Latin1.lexeme_char lexbuf 0);
      string lexbuf

and quoted_string delim lexbuf =
  match%sedlex lexbuf with
  | newline ->
      update_loc lexbuf None 1 false 0;
      store_lexeme lexbuf;
      quoted_string delim lexbuf
  | eof ->
      is_in_string := false;
      raise (Error (Unterminated_string, !string_start_loc))
  | "|", Star lowercase, "}" ->
      let edelim = Sedlexing.Latin1.lexeme lexbuf in
      let edelim = String.sub edelim 1 (String.length edelim - 2) in
      if delim = edelim then ()
      else (store_lexeme lexbuf; quoted_string delim lexbuf)
  | _ ->
      store_string_char(Sedlexing.Latin1.lexeme_char lexbuf 0);
      quoted_string delim lexbuf

and skip_hash_bang lexbuf =
  match%sedlex lexbuf with
  | "#!", Star (Compl '\n'), '\n', Star (Compl '\n'), "\n!#\n" ->
      update_loc lexbuf None 3 false 0
  | "#!", Star (Compl '\n'), '\n' ->
      update_loc lexbuf None 1 false 0
  | "" -> ()
  | _ -> assert false

let token_with_comments lexbuf =
  match !preprocessor with
  | None -> token lexbuf
  | Some (_init, preprocess) -> preprocess token lexbuf

type newline_state =
  | NoLine (* There have been no blank lines yet. *)
  | NewLine
  (* There have been no blank lines, and the previous
     token was a newline. *)
  | BlankLine (* There have been blank lines. *)

type doc_state =
  | Initial  (* There have been no docstrings yet *)
  | After of docstring list
  (* There have been docstrings, none of which were
     preceded by a blank line *)
  | Before of docstring list * docstring list * docstring list
  (* There have been docstrings, some of which were
     preceded by a blank line *)

and docstring = Docstrings.docstring

let token lexbuf =
  let _, post_pos = Sedlexing.lexing_positions lexbuf in
  let attach lines docs pre_pos =
    let open Docstrings in
    match docs, lines with
    | Initial, _ -> ()
    | After a, (NoLine | NewLine) ->
        set_post_docstrings post_pos (List.rev a);
        set_pre_docstrings pre_pos a;
    | After a, BlankLine ->
        set_post_docstrings post_pos (List.rev a);
        set_pre_extra_docstrings pre_pos (List.rev a)
    | Before(a, f, b), (NoLine | NewLine) ->
        set_post_docstrings post_pos (List.rev a);
        set_post_extra_docstrings post_pos
          (List.rev_append f (List.rev b));
        set_floating_docstrings pre_pos (List.rev f);
        set_pre_extra_docstrings pre_pos (List.rev a);
        set_pre_docstrings pre_pos b
    | Before(a, f, b), BlankLine ->
        set_post_docstrings post_pos (List.rev a);
        set_post_extra_docstrings post_pos
          (List.rev_append f (List.rev b));
        set_floating_docstrings pre_pos
          (List.rev_append f (List.rev b));
        set_pre_extra_docstrings pre_pos (List.rev a)
  in
  let rec loop lines docs lexbuf =
    match token_with_comments lexbuf with
    | COMMENT (s, loc) ->
        add_comment (s, loc);
        let lines' =
          match lines with
          | NoLine -> NoLine
          | NewLine -> NoLine
          | BlankLine -> BlankLine
        in
        loop lines' docs lexbuf
    | EOL ->
        let lines' =
          match lines with
          | NoLine -> NewLine
          | NewLine -> BlankLine
          | BlankLine -> BlankLine
        in
        loop lines' docs lexbuf
    | DOCSTRING doc ->
        Docstrings.register doc;
        add_docstring_comment doc;
        let docs' =
          if Docstrings.docstring_body doc = "/*" then
            match docs with
            | Initial -> Before([], [doc], [])
            | After a -> Before (a, [doc], [])
            | Before(a, f, b) -> Before(a, doc :: b @ f, [])
          else
            match docs, lines with
            | Initial, (NoLine | NewLine) -> After [doc]
            | Initial, BlankLine -> Before([], [], [doc])
            | After a, (NoLine | NewLine) -> After (doc :: a)
            | After a, BlankLine -> Before (a, [], [doc])
            | Before(a, f, b), (NoLine | NewLine) -> Before(a, f, doc :: b)
            | Before(a, f, b), BlankLine -> Before(a, b @ f, [doc])
        in
        loop NoLine docs' lexbuf
    | tok ->
        attach lines docs (lexeme_start_p lexbuf);
        tok
  in
  loop NoLine Initial lexbuf

let init () =
  is_in_string := false;
  comment_start_loc := [];
  comment_list := [];
  match !preprocessor with
  | None -> ()
  | Some (init, _preprocess) -> init ()

let set_preprocessor init preprocess =
  escaped_newlines := true;
  preprocessor := Some (init, preprocess)
