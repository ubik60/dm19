module H = Tyxml.Html
let default v = function 
| None -> v 
| Some x -> x

let bind f = function
| None -> None 
| Some x -> Some (f x)


let consopt = function
| None -> (function l -> l)
| Some x -> (function  
        | None -> Some [x] 
        | Some l -> Some (x :: l))

let aattrs ?style ?klass a = 
    a 
    |> consopt (bind H.a_style style) 
    |> consopt (bind H.a_class (bind (fun x -> [x]) klass))
let p ?a ?style ?klass l = H.p ?a:(aattrs ?style ?klass a) l 
let ol ?a ?style ?klass l = H.ol ?a:(aattrs ?style ?klass a) l 
let ul ?a ?style ?klass l = H.ul ?a:(aattrs ?style ?klass a) l 
let li ?a ?style ?klass l = H.li ?a:(aattrs ?style ?klass a) l 
let a ?a ?style ?klass l = H.a ?a:(aattrs ?style ?klass a) l 
let div ?a ?style ?klass l = H.div ?a:(aattrs ?style ?klass a) l 
let span ?a ?style ?klass l = H.span ?a:(aattrs ?style ?klass a) l 
let code ?a ?style ?klass l = H.code ?a:(aattrs ?style ?klass a) l 
let pre ?a ?style ?klass l = H.pre ?a:(aattrs ?style ?klass a) l 

let txt ?a s = H.txt s
let td ?a ?style ?klass l = H.td ?a:(aattrs ?style ?klass a) l 
let th ?a ?style ?klass l = H.th ?a:(aattrs ?style ?klass a) l 
let tr ?a ?style ?klass l = H.tr ?a:(aattrs ?style ?klass a) l 
let table ?caption ?columns ?thead ?tfoot ?a ?style ?klass l = 
    H.table ?a:(aattrs ?style ?klass a) ?caption ?columns ?thead ?tfoot l

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