let mapconcat ?(f=id) list = List.fold_left (fun s d -> s ^ (f d)) "" list
let td text = "<td>" ^ text ^ "</td>"
let th ?(style="width:5ex;") text = "<th style=\""^style^"\">" ^ text ^ "</th>"
let tr text = "<tr>" ^ text ^ "</tr>\n"
let table text = "<table>\n" ^ text ^ "\n</table>\n"
(** [td text], [tr text], [th text] och [table text] kapsalr in [text] i motsvarande htmlelement *)
let ltable ?(hlist=[]) ll = 
    let header = hlist |> mapconcat ~f:(fun s -> th ("&nbsp;" ^ s ^ "&nbsp")) in
    let body = ll |> List.map (mapconcat ~f:td) |> mapconcat ~f:tr
    in 
      (if header <> "" then (tr header) ^ "\n" else "") ^ body |> table