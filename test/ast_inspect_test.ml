open Ccaml

let () =
  let rec run =
    let delimiter = "==========" in
    function
    | [] -> ()
    | program :: rest ->
        let tokens = Lexer.tokenize program in
        let ast = Parser.parse tokens in
        print_endline program;
        print_string (Ast.inspect ast);
        print_endline delimiter;
        run rest
  in
  run
    [
      (* with a single return statement with value *)
      "int main() {\n    return 29;\n}";
      (* with no return statement *)
      "int main() {}";
      (* with multiple return statements *)
      "int main() {\n    return 2;\n    return 100;\n}";
      (* with multiple return statements with - *)
      "int main() {\n    return -42;\n}";
      (* with multiple return statements with ~ *)
      "int main() {\n    return ~42;\n}";
      (* with multiple return statements with ! *)
      "int main() {\n    return !42;\n}";
      (* with multiple return statements with !~-~-! *)
      "int main() {\n    return !~-~-!42;\n}";
      (* with binary operation *)
      "int main() {\n    return 1 + 2;\n}";
      "int main() {\n    return (1 + 2) - (2 - 9);\n}";
      "int main() {\n    return 1 + 2 * 8;\n}";
      "int main() {\n    return 1 + 2 / 7;\n}";
      "int main() {\n    return (1 + 2) * 9;\n}";
      "int main() {\n    return 1 / 0;\n}";
    ]
