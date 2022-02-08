let rec token buf =
  match%sedlex buf with
  | eof -> print_endline "\tEnd"
  | (Star white_space), ((Star (Compl white_space)) as text), (Star white_space) ->
     print_endline text; token buf
  | any -> print_endline "other"; token buf
  | _ -> failwith "Internal failure: Reached impossible place"


let () =
  let lexbuf = Sedlexing.Utf8.from_string "It takes all the running you can do, to keep in the same place." in
  token lexbuf
