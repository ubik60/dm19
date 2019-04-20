open Format

type fmt = Format.formatter

module type Order = Set.OrderedType

module type S = sig
  include Stdlib.Set.S

  val pp: ?maxnum:int -> pp_elt:(fmt -> elt -> unit) -> fmt -> t -> unit

  (** [let pps = pp ~maxnum:n pp_elt:ppe] returns a pretty printer that can be
     installed in the toplevel with [#install_printer pps;;] *)
end

functor Make(Elt:Order) : S = struct
  include Stdlib.Set.Make(Elt) with type elt := Elt.t

  let pp ?(maxnum=1000) ~pp_elt:pp_elt fmt set =
      fprintf fmt "@[<1>{";
      let prelt elt count =
        let () = match count with
            | 0 -> pp_elt fmt elt
            | n when n < maxnum -> fprintf fmt ",@;%a" pp_elt elt
            | n  ->
                if n = maxnum then fprintf fmt " ...@;" else ()
        in count+1
      in
          S.fold prelt set 0 |> ignore;
      fprintf fmt "}@]"
end


module Int:Order   = struct type t = int    let compare = compare end
module Str:Order   = struct type t = string let compare = compare end
module Flt:Order   = struct type t = float  let compare = compare end

module Sint    with type elt = int    = Make(Int)
module Sstr    with type elt = string = Make(Str)
module Sfloat  with type elt = float = Make(Flt)


(** *)
let pp = pp_t ~maxnum:300 ~pp_elt:pp_print_int;;
#install_printer pp;;

(** [diff a b] ger mängdifferensen av elementen i [a] men inte i [b]. *)
let diff a b = S.fold
    (fun x a' -> if not (S.mem x b) then S.add x a' else a') a S.empty

(* grundmängd *)
let setU = S.of_list([0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11])
(* Komplement finns inte med *)
let compl s = diff setU s
let setA = S.of_list([0; 1; 2; 3; 4; 5])
let setB = S.of_list([5; 6; 7])
let setC = S.of_list([3; 5; 8])

(** En funktion som skapar en mängd av heltalsintervallet från [m] till och med [n-1]. *)
let rec add_range n m t =
  if n < m then S.add n t |> add_range (n+1) m else t
let range n m = add_range n m S.empty

let _ = range 1 700
