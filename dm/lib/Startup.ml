
#use "topfind";;
#require "tyxml, jupyter.notebook"

let hstr = {|
<h1>SVG in Notebook </h1>
<object id="demo-tiger" type="image/svg+xml" data="https://cdn.rawgit.com/ariutta/svg-pan-zoom/master/demo/tiger.svg" 
style="width: 400px; height: 400px; border:1px solid black; ">
Your browser does not support SVG
</object>
|}
in Jupyter_notebook.display "text/html"  hstr

let style = "<style>.svg{width:50% !important;height:50% !important;}</style>" in 
Jupyter_notebook.display "text/html"  style

let hstr = {|
<h1>SVG in Notebook </h1>
<img id="blue" type="image/svg+xml" src="Blue.svg" 
style="width: 400px; height: 400px; border:1px solid black; "/>
|}
in Jupyter_notebook.display "text/html"  hstr

let ic = open_in "Blue.svg"

let read_file ic = 
  let n = in_channel_length ic in 
   let str = really_input_string ic n in 
   close_in ic; str

let bluestr = really_input_string ic n
let _ = close_in ic

Jupyter_notebook.display "image/svg+xml" bluestr

open Tyxml

let html_to_string t = Format.asprintf "%a" (Html.pp_elt ()) t
let phtml t = html_to_string t |> Jupyter_notebook.display "text/html" 

let t = Html.(a ~a:[a_href "ocaml.org"] [txt "OCaml!"])

phtml t

let html = {|
<h1>Inline SVG in Notebook </h1>
<svg id="blue" type="image/svg+xml" src="Blue.svg" 
style="width: 400px; height: 400px; border:1px solid black; ">
|} ^ bluestr ^ {|
</svg>
|}
in Jupyter_notebook.display "text/html"  html

let hstr = {|
<p> \(\sum_{i=1}^{\infty} x_i\) </p>
|}
in Jupyter_notebook.display "text/html"  hstr

let ic = Unix.open_process_in("dot -Tsvg digraf.dot")

let dotgraf = read_file ic 
