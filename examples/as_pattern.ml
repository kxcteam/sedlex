let string_of_uchars us =
   String.of_seq (Array.to_seq (Array.map Uchar.to_char us))

let rec token1 buf =
  match%sedlex buf with
  | eof -> print_endline "\tEnd"
  | white_space -> print_endline "\tWhitespace"; token1 buf
  | (Plus ('a' .. 'z' | 'A' .. 'Z')) as text ->
     print_endline (string_of_uchars text); token1 buf
  | (',' | '.') as x ->
     print_endline (string_of_uchars x); token1 buf
  | any -> print_endline "other"; token1 buf
  | _ -> failwith "Internal failure: Reached impossible place"

let rec token2 buf =
  match%sedlex buf with
  | eof -> print_endline "\tEnd"
  | white_space -> print_endline "\tWhitespace"; token2 buf
  | "a" as x ->
     print_endline (string_of_uchars x); token2 buf
  | "b" as x ->
     print_endline (string_of_uchars x); token2 buf
  | "cde" as x ->
     print_endline (string_of_uchars x); token2 buf
  | ("f" as x), "g" ->
     print_endline (string_of_uchars x); token2 buf
  | "h", ("i" as x) ->
     print_endline (string_of_uchars x); token2 buf
  | ("j" as x | "k" as x) ->
     print_endline (string_of_uchars x); token2 buf
  | ("l" | "m") as x ->
     print_endline (string_of_uchars x); token2 buf
  | (Star "n" as x), (Star "o" as y) ->
     print_endline (string_of_uchars x^"+"^string_of_uchars y); token2 buf
  | (Plus "p" as x), (Plus "q" as y) ->
     print_endline (string_of_uchars x^"+"^string_of_uchars y); token2 buf
  | any -> print_endline "other"; token1 buf
  | _ -> failwith "Internal failure: Reached impossible place"


let () =
  token1 (Sedlexing.Utf8.from_string "It takes all the running you can do, to keep in the same place.");
  token2 (Sedlexing.Utf8.from_string "a b cde fg hi j k l m n o nn no oo nnn nno noo ooo pq ppq pqq pppq ppqq pqqq")



(*
let rec token buf =
  match%sedlex buf with
  | eof -> print_endline "\tEnd"
  | white_space -> print_endline "\tWhitespace"; token buf
  | (Star 'a' as a), ('b' as b) -> print_endline (string_of_uchars a^"\t"^string_of_uchars b); token buf
  | any -> print_endline "Other"; token buf
  | _ -> failwith "Internal failure: Reached impossible place"

let () =
  let lexbuf = Sedlexing.Utf8.from_string "b ab aab aaab aaaab" in
  token lexbuf
*)
