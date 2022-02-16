let rec token buf =
  match%sedlex buf with
  | eof -> print_endline "\tEnd"
  | white_space -> print_endline "\tWhitespace"; token buf
  | ((Plus ('a' .. 'z' | 'A' .. 'Z')) as text, (Star (white_space | ',' | '.'))) ->
     print_string "as-pattern text:\t";
     print_endline (String.of_seq (Array.to_seq (Array.map Uchar.to_char text)));
     token buf
  | (',' | '.') as x ->
     print_string "as-pattern x:\t";
     print_endline (String.of_seq (Array.to_seq (Array.map Uchar.to_char x)));
     token buf
  | any -> print_endline "other"; token buf
  | _ -> failwith "Internal failure: Reached impossible place"


let () =
  let lexbuf = Sedlexing.Utf8.from_string "It takes all the running you can do, to keep in the same place." in
  token lexbuf
