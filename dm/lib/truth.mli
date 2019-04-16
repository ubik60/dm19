(** En typ ['a boolarity] för att hantera booleska funktioner med olika aritet. *)
type 'a boolarity =
        B1 : bool boolarity
      | B2 : (bool * bool) boolarity
      | B3 : (bool * bool * bool) boolarity
      | B4 : (bool * bool * bool * bool) boolarity
      | B5 : (bool * bool * bool * bool * bool) boolarity
      | B6 : (bool * bool * bool * bool * bool * bool) boolarity

(** [truthtable ~hlist tag flist]
    skapar en sanningstabell där de sista kolonnerna ger funktionerna i [flist].
    Alla funktioner i [flist] måste ha aritet enligt [tag] *)
val table :?hlist:string list -> 'a boolarity -> ('a -> bool) list -> string
