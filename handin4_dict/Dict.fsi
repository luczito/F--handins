module handin4_dict.Dict

type Dict

val empty : unit -> Dict

val insert : string -> Dict -> Dict

val lookup : string -> Dict -> bool  