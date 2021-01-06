module SM = Map.Make(String)

let rec generalize = function
  | `Null -> `Unit
  | `Bool _ -> `Bool
  | `Int _ | `Intlit _ -> `Int
  | `Float _ -> `Float
  | `String _ -> `String
  | `List x -> `List (generalize_list x)
  | `Assoc x -> `Assoc (List.to_seq x |> Seq.map (fun (k, v) -> k, generalize v) |> SM.of_seq)
  | `Tuple x -> `Tuple (List.map generalize x)
  | `Variant (k, x) -> `Variant (SM.singleton k (Option.map generalize x))
and generalize_assoc x y =
  SM.merge begin fun _k x y ->
    match x, y with
    | Some x, Some y -> Some (gen2 x y)
    | Some (`Option x | x), None | None, (Some (`Option x | x)) -> Some (`Option x)
    | None, None -> assert false
  end x y
and generalize_list : Yojson.Safe.t list -> 'a = function
  | [] -> `Empty
  | hd :: tl ->
  List.fold_left begin fun a (x : Yojson.Safe.t) ->
    gen2 a (generalize x)
  end (generalize hd) tl
and generalize_variant x y =
  SM.merge begin fun _k x y ->
    match x, y with
    | Some Some x, Some Some y -> Some (Some (gen2 x y))
    | Some _ as x, None | None, (Some _ as x) -> x
    | Some None as x, Some None -> x
    | Some Some _, Some None | Some None, Some Some _ -> Some (Some `Mixed)
    | None, None -> assert false
  end x y
and gen2 x y =
  match x, y with
  | `Unit, `Unit -> `Unit
  | `Unit, x | x, `Unit -> `Nullable x
  | `Bool, `Bool -> `Bool
  | `Int, `Int -> `Int
  | `Int, `Float | `Float, `Int | `Float, `Float -> `Float
  | `String, `String -> `String
  | `List _ as fixme, `List _ -> fixme
  | `Assoc x, `Assoc y -> `Assoc (generalize_assoc x y)
  | `Tuple x, `Tuple y -> `Tuple (List.map2 gen2 x y) (* FIXME different length *)
  | `Variant x, `Variant y -> `Variant (generalize_variant x y)
  | `Nullable x, (`Nullable y | y) | x, `Nullable y -> `Nullable (gen2 x y)
  | `Option x, (`Option y | y) | x, `Option y -> `Option (gen2 x y)
  | _, _ -> `Mixed

let paren l r x = String.concat " " [ l; x; r; ]

let rec string_of_gen = function
  | `Empty -> "_"
  | `Unit -> "unit"
  | `Bool -> "bool"
  | `Int -> "int"
  | `Float -> "float"
  | `String -> "string"
  | `List x -> wrap x "list"
  | `Assoc x when SM.is_empty x -> "{ !empty! }"
  | `Assoc x ->
    SM.bindings x |>
    List.map (fun (k, v) -> String.concat " " [ k; ":"; string_of_gen v; ] ^ ";") |>
    String.concat " " |>
    paren "{" "}"
  | `Tuple x ->
    let res = String.concat " * " (List.map string_of_gen x) in
    (match x with _ :: _ :: _ -> String.concat "" [ "("; res; ")"; ] | _ :: _ -> res | [] -> "()")
  | `Variant x ->
    SM.bindings x |>
    List.map begin fun (k, x) ->
      String.concat " of " (k :: match x with Some x -> [ string_of_gen x; ] | None -> [])
    end |>
    String.concat " | " |>
    paren "[" "]"
  | `Nullable x -> wrap x "nullable"
  | `Option x -> wrap x "option"
  | `Mixed ->
    "!mixed!"
and wrap x y =
  String.concat " " [ string_of_gen x; y; ]

let () =
  let str = IO.read_all (IO.input_channel stdin) in
  let json = Yojson.Safe.from_string str in
  let gen = generalize json in
  print_endline ("type t = " ^ string_of_gen gen)
