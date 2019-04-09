
module Jup : sig 
    type display_id
    val show : string -> display_id
end

= struct 
include Jupyter_notebook
(** [show htmlstring] visa formatterad HTML som output i notebook *)
let show = display "text/html"
let show_svg = display "image/svg+xml"
end