structure LevelOperators =
struct
  datatype level_cont =
      LSUP0
    | LSUP1
    | LSUCC
end

structure SimpleLevelV : ABT_SIMPLE_OPERATOR =
struct
  open LevelOperators SortData ArityNotation
  structure Ar = RedPRLAtomicArity

  type t = int

  infix <> ->>
  fun arity _ = [] ->> LVL

  val eq : t * t -> bool = op=
  val toString = Int.toString
end

structure SimpleLevelK : ABT_SIMPLE_OPERATOR =
struct
  open LevelOperators SortData ArityNotation
  structure Ar = RedPRLAtomicArity

  type t = level_cont

  infix <> ->>
  val arity =
    fn LSUP0 => [[] * [] <> LVL] ->> LVL
     | LSUP1 => [[] * [] <> LVL] ->> LVL
     | LSUCC => [] ->> LVL

  val eq : t * t -> bool = op=

  val toString =
    fn LSUP0 => "lsup0"
     | LSUP1 => "lsup1"
     | LSUCC => "lsucc"
end

structure LevelV = AbtSimpleOperator (SimpleLevelV)

structure LevelK :
sig
  include ABT_OPERATOR
  val input : 'i t -> RedPRLAtomicArity.sort
end =
struct
  structure O = AbtSimpleOperator (SimpleLevelK)
  open O SortData LevelOperators

  val input =
    fn LSUP0 => LVL
     | LSUP1 => LVL
     | LSUCC => LVL
end
