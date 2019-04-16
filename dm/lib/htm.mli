(** 
  [ltable stringll ~hlist ~vlist ] 
  Skapa en formatterad HTML tabell från en lista av listor av strängar.
  De valfria argumenten [~hlist] och [~vlist] anger kolumn respektive rad huvuden.   *)
val ltable:  ?vlist:string list -> ?hlist:string list -> (string list list) -> string let id = fun x -> x