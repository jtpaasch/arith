let () =
  Cli.cli ();
  let expr = Cli.expr () in
  Printf.printf "Expression: '%s'\n%!" expr;
  let tokens = Lexer.lex expr in
  List.iter (fun x -> Printf.printf "Token: '%s'\n%!" (Token.to_string x)) tokens;
  let tree = Parser.reduce tokens in
  Printf.printf "Tree: '%s'\n%!" (Parser.to_string tree)

