module Exn : sig
  val failwithf : ('a, unit, string, string, string, 'b) format6 -> 'a
  (** Fail with formatted message. *)
end

module Compare : sig
  val ( <>? ) : int -> (unit -> int) -> int
  (** Composition of comparisons: [compare a b <>? fun () -> compare x y]. *)

  val the : ('a -> 'b) -> ('b -> 'b -> int) -> 'a -> 'a -> int

  module Pair (Lhs : Set.OrderedType) (Rhs : Set.OrderedType) :
    Set.OrderedType with type t = Lhs.t * Rhs.t
end

module ListExt : sig
  val equal_with : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
  val compare_with : ('a -> 'a -> int) -> 'a list -> 'a list -> int
end

module UTF8 : sig
  val to_uchar_array : string -> Uchar.t array
  (** Convert UTF-8 string to an array of Unicode characters. *)
end

val failwithf : ('a, unit, string, string, string, 'b) format6 -> 'a
(** Fail with formatted message. *)

val ( <>? ) : int -> (unit -> int) -> int
(** Composition of comparisons: [compare a b <>? fun () -> compare x y]. *)
