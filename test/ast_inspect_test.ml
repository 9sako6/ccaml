let delimiter = "=========="

(* with a single return statement with value *)
let program = "int main() {\n    return 29;\n}"
let () = print_endline program
let tokens = Lexer.tokenize program
let ast = Parser.parse tokens
let () = print_string (Ast.inspect ast)
let () = print_endline delimiter

(* with a single return statement without value *)
let program = "int main() {\n    return;\n}"
let () = print_endline program
let tokens = Lexer.tokenize program
let ast = Parser.parse tokens
let () = print_string (Ast.inspect ast)
let () = print_endline delimiter

(* with no return statement *)
let program = "int main() {}"
let () = print_endline program
let tokens = Lexer.tokenize program
let ast = Parser.parse tokens
let () = print_string (Ast.inspect ast)
let () = print_endline delimiter

(* with multiple return statements *)
let program = "int main() {\n    return;\n    return 100;\n}"
let () = print_endline program
let tokens = Lexer.tokenize program
let ast = Parser.parse tokens
let () = print_string (Ast.inspect ast)
let () = print_endline delimiter
