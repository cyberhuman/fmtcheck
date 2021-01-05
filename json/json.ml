let rec generalize = function
  | `Null -> `Unit
  | `Bool _ -> `Bool
  | `Int _ | `Intlit _ -> `Int
  | `Float _ -> `Float
  | `String _ -> `String
  | `List x -> `List (generalize_list x)
  | `Assoc x -> `Assoc (List.map (fun (k, v) -> k, generalize v) x)
  | `Tuple x -> `Tuple (List.map generalize x)
  | `Variant (k, x) -> `Variant [ k, Option.map generalize x; ]
and generalize_list : Yojson.Safe.t list -> 'a = function
  | [] -> `Empty
  | hd :: tl ->
  List.fold_left begin fun a (x : Yojson.Safe.t) ->
    gen2 a (generalize x)
  end (generalize hd) tl
and gen2 x y =
  match x, y with
  | `Unit, `Unit -> `Unit
  | `Unit, x | x, `Unit -> `Option x
  | `Bool, `Bool -> `Bool
  | `Int, `Int -> `Int
  | `Int, `Float | `Float, `Int | `Float, `Float -> `Float
  | `String, `String -> `String
  | `List _ as fixme, `List _ -> fixme
  | `Assoc _ as fixme, `Assoc _ -> fixme
  | `Tuple x, `Tuple y -> `Tuple (List.map2 gen2 x y) (* FIXME different length *)
  | `Variant x, `Variant y -> `Variant (x @ y) (* FIXME Map.Make(String).merge recursive generalize values *)
  | _, _ -> `Mixed

let paren l r x = String.concat " " [ l; x; r; ]

let rec string_of_gen = function
  | `Empty -> "_"
  | `Unit -> "unit"
  | `Bool -> "bool"
  | `Int -> "int"
  | `Float -> "float"
  | `String -> "string"
  | `List x -> String.concat " " [ string_of_gen x; "list"; ]
  | `Assoc x ->
    List.map (fun (k, v) -> String.concat " " [ k; ":"; string_of_gen v; ] ^ ";") x |>
    String.concat " " |>
    paren "{" "}"
  | `Tuple x ->
    let res = String.concat " * " (List.map string_of_gen x) in
    (match x with _ :: _ :: _ -> String.concat "" [ "("; res; ")"; ] | _ -> res)
  | `Variant x ->
    List.map begin fun (k, x) ->
      String.concat " of " (k :: match x with Some x -> [ string_of_gen x; ] | None -> [])
    end x |>
    String.concat " | " |>
    paren "[" "]"
  | `Option x ->
    String.concat " " [ string_of_gen x; "option"; ]
  | `Mixed ->
    "!mixed!"

let () =
  let str = IO.read_all (IO.input_channel stdin) in
  let json = Yojson.Safe.from_string str in
  let gen = generalize json in
  print_endline ("type t = " ^ string_of_gen gen)
