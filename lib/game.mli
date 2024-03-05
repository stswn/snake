type state = Waiting | Started | Paused | GameOver [@@deriving show]
type cell = { x : int; y : int } [@@deriving show]
type snake = cell list [@@deriving show]
type t [@@deriving show]

val init : width:int -> height:int -> t
val start : t -> t
val turn_right : t -> t
val turn_left : t -> t
val set_paused : t -> bool -> t
val get_paused : t -> bool
val get_snake : t -> snake
val get_food : t -> cell
val advance : t -> t
val get_state : t -> state
val get_round : t -> int
val get_points : t -> int
