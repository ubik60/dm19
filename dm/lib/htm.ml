module H = Tyxml.Html5
let mapconcat ?(f=id) list = List.fold_left (fun s d -> s ^ (f d)) "" list

let table = H.table 
let td = H.td  
let th ?(style="width:7ex;") text = "<th style=\""^style^"\">" ^ text ^ "</th>"
let tr text = "<tr>" ^ text ^ "</tr>\n"
let table text = "<table>\n" ^ text ^ "\n</table>\n"
(** [td text],
    [tr text], [th text] och [table text] kapslar in [text] i motsvarande htmlelement *)

let ltable ?vlist ?hlist ll =
  let (ll,hlist) =
    if vlist <> [] then (List.map2 (fun s l -> s :: l) vlist ll, "" :: hlist)
    else ll
  in
  let header = hlist |> mapconcat ~f:th in
  let body = ll |> List.map (mapconcat ~f:td)
             |> mapconcat ~f:tr
  in (if hlist <> [] then (tr header) else "") ^ body
     |> table
