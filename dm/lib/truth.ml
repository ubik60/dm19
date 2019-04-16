(** En typ ['a boolarity] för att hantera booleska funktioner med olika aritet. *)
type 'a boolarity =
  | B1 : bool boolarity
  | B2 : (bool*bool) boolarity
  | B3 : (bool*bool*bool) boolarity
  | B4 : (bool*bool*bool*bool) boolarity
  | B5 : (bool*bool*bool*bool*bool) boolarity
  | B6 : (bool*bool*bool*bool*bool*bool) boolarity

(** [arity tag] anger antalet komponenter i tupeln som anges av [tag] *)
let arity: type a. a boolarity -> int =
  function B1 -> 1 | B2 -> 2 | B3 -> 3 | B4 -> 4 | B5 -> 5
         | _ -> failwith "Not implemented"

(** [convert_from_list tag lst] gör en tupel av elementen i listan [lst] *)
let convert_from_list: type a. (a boolarity) -> (bool list) -> a = function
  | B1 -> (function [p] -> p | _ -> failwith "Bad type")
  | B2 -> (function [p;q] -> (p,q) | _ -> failwith "Bad type")
  | B3 -> (function [p;q;r] -> (p,q,r) | _ -> failwith "Bad type")
  | B4 -> (function [p;q;r;s] -> (p,q,r,s) | _ -> failwith "Bad type")
  | B5 -> (function [p;q;r;s;t] -> (p,q,r,s,t) | _ -> failwith "Bad type")
  | _ -> failwith "Not implemented"

(** [convert_to_list tag tupel] gör en lista av elementen i tupeln [tupel] *)
let convert_to_list: type a. (a boolarity) -> a -> (bool list) = function
  | B1 -> fun p -> [p]
  | B2 -> fun (p,q) -> [p; q]
  | B3 -> fun (p,q,r) -> [p;q;r]
  | B4 -> fun (p,q,r,s) -> [p;q;r;s]
  | B5 -> fun (p,q,r,s,t) -> [p;q;r;s;t]
  | _ -> failwith "Not implemented"

(** [gen n] skapar en lista av alla $2^n$ bool-listor av längd [n] *)
let rec gen n =
  if n <= 1 then
    [ [true]; [false] ]
  else
    let ll = gen (n-1) in
    List.map (fun t -> List.map (fun lst -> (t :: lst)) ll) [true; false]
    |> List.concat

(** [truthtable ~hlist tag flist]
    skapar en sanningstabell där de sista kolonnerna ger funktionerna i [flist].
    Alla funktioner i [flist] måste ha aritet enligt [tag] *)
let table : type a. ?hlist:(string list) ->  (a boolarity) -> ((a->bool) list) -> string =
  fun ?(hlist=[]) tag flist ->
  gen (arity tag) |> List.map (convert_from_list tag)
  |> List.map (fun a -> (convert_to_list tag a) @ (List.map (fun f -> f a) flist))
  |> List.map (List.map (fun b -> if b then "1" else "0"))
  |> Htm.ltable ~hlist
