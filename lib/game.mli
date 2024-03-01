type state = Waiting | Started | Paused | GameOver [@@deriving show]
type cell = { x : int; y : int } [@@deriving show]
type direction = Left | Up | Right | Down [@@deriving show]
type snake = cell list [@@deriving show]
type t [@@deriving show]

val init : width:int -> height:int -> t
val start : t -> t
val change_direction : t -> direction -> t
val set_paused : t -> bool -> t
val get_paused : t -> bool
val get_snake : t -> snake
val get_food : t -> cell
val advance : t -> t
val get_round : t -> int
val get_points : t -> int