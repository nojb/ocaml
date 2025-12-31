val parse_expression: (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> Parsetree.expression
val parse_pattern: (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> Parsetree.pattern
val parse_module_type: (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> Parsetree.module_type
val parse_module_expr: (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> Parsetree.module_expr
val parse_core_type: (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> Parsetree.core_type

val implementation: (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> Parsetree.structure
val interface: (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> Parsetree.signature

val toplevel_phrase: (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> Parsetree.toplevel_phrase

val use_file: (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> Parsetree.toplevel_phrase list
