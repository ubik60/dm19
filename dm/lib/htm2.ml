module H = Tyxml.Html5

H.
let addstyle a styl = match styl,alist with 
| None,_ -> alist 
| Some s,None -> [H.a_style s] 
| Some s,alist -> 
let txt s = H.txt s
let td ?a l = H.td ~a l
let th ?a l = H.th ~a l
let table ?a l = H.table ~a l

let printelt elt = 
    Format.asprintf "%a@." (pp_elt ~indent:true ()) elt
    

let ftable ?caption 
    (f:'a option ->'b option->'c elt ) (rows:'a list) (cols:'b list) = 
    let map = List.map in
    let trone = tr ((f None None) :: map (fun c -> f None (Some c)) cols)
    in printelt trone

                        
    (*)
    table
        [
        map (fun row -> 
        tr [f (Some row) None] @ (map (fun col -> f (Some row) (Some col)) cols)
    ] 
*)