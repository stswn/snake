type state = Waiting | Started | Paused | GameOver [@@deriving show]
type cell = { x : int; y : int } [@@deriving show]
type snake = cell list [@@deriving show]

[@@@warning "-32"]

type direction = Left | Up | Right | Down [@@deriving show, enum]

[@@@warning "+32"]

type command = TurnLeft | TurnRight [@@deriving show]
type field = Empty | Food | Snake [@@deriving show]
type board = field array array [@@deriving show]

type t = {
  width : int;
  height : int;
  state : state;
  round : int;
  direction : direction;
  snake : cell list;
  food : cell;
  commands : command list;
}
[@@deriving show]

(* Finds free cell to put food  or initial snake in *)
let find_free_cell ?(random_int = Random.int) width height occupied =
  let free_cells = (width * height) - List.length occupied in
  if free_cells <= 0 then Option.none
  else
    let start_idx = random_int free_cells in
    let rec find_cell cell idx =
      let cell_occupied = List.exists (fun c -> c = cell) occupied in
      let next_cell =
        if cell.x < width - 1 then { cell with x = cell.x + 1 }
        else if cell.y < height - 1 then { x = 0; y = cell.y + 1 }
        else { x = 0; y = 0 }
      in
      if cell_occupied then find_cell next_cell idx
      else if idx = 0 then cell
      else find_cell next_cell (idx - 1)
    in
    Option.some @@ find_cell { x = 0; y = 0 } start_idx

let%test "no free cells" =
  find_free_cell 2 2
    [ { x = 0; y = 0 }; { x = 0; y = 1 }; { x = 1; y = 0 }; { x = 1; y = 1 } ]
  = Option.none

let%test "last free cell" =
  find_free_cell 2 2 [ { x = 0; y = 0 }; { x = 0; y = 1 }; { x = 1; y = 0 } ]
  = Option.some { x = 1; y = 1 }

let%test "omit occupied cells (0)" =
  find_free_cell ~random_int:(fun _ -> 0) 2 2 [ { x = 0; y = 0 } ]
  = Option.some { x = 1; y = 0 }

let%test "omit occupied cells (1)" =
  find_free_cell ~random_int:(fun _ -> 1) 2 2 [ { x = 1; y = 0 } ]
  = Option.some { x = 0; y = 1 }

let init ~width ~height =
  let snake_start = find_free_cell width height [] |> Option.get in
  let snake = [ snake_start ] in
  let food = find_free_cell width height snake |> Option.get in
  let direction = Random.int 4 |> direction_of_enum |> Option.get in
  {
    width;
    height;
    state = Waiting;
    round = 1;
    direction;
    snake;
    food;
    commands = [];
  }

let start game =
  match game.state with Waiting -> { game with state = Started } | _ -> game

let when_started game updated_game =
  match game.state with Started -> updated_game | _ -> game

let turn_right game =
  when_started game @@ { game with commands = game.commands @ [ TurnRight ] }

let turn_left game =
  when_started game @@ { game with commands = game.commands @ [ TurnLeft ] }

let set_paused paused game =
  match (game, paused) with
  | ({ state = Started; _ } as s), true -> { s with state = Paused }
  | ({ state = Paused; _ } as s), false -> { s with state = Started }
  | s, _ -> s

let get_paused game = game.state = Paused
let get_snake game = game.snake
let get_food game = game.food

let get_board game =
  let board = Array.make_matrix game.width game.height Empty in
  board.(game.food.x).(game.food.y) <- Food;
  List.iter (fun s -> board.(s.x).(s.y) <- Snake) game.snake;
  board

let nmod x y = ((x mod y) + y) mod y

let drop_last snake =
  let rec drop_last_reversed s acc =
    match (s, acc) with
    | [], acc -> acc
    | _ :: [], acc -> acc
    | x :: xs, acc -> drop_last_reversed xs (x :: acc)
  in
  drop_last_reversed snake [] |> List.rev

let next_direction current_direction command =
  let rot =
    match command with Some TurnRight -> 1 | Some TurnLeft -> 3 | None -> 0
  in
  (direction_to_enum current_direction + rot) mod 4
  |> direction_of_enum |> Option.get

let hd_opt = function [] -> Option.none | x :: _ -> Option.some x
let tl_sf = function [] -> [] | _ :: xs -> xs

let advance game =
  when_started game
  @@
  let current_head = List.hd game.snake in
  let command = hd_opt game.commands in
  let direction = next_direction game.direction command in
  let next_head =
    match direction with
    | Left -> { current_head with x = nmod (current_head.x - 1) game.width }
    | Up -> { current_head with y = nmod (current_head.y - 1) game.height }
    | Down -> { current_head with y = nmod (current_head.y + 1) game.height }
    | Right -> { current_head with x = nmod (current_head.x + 1) game.width }
  in
  let next_round = game.round + 1 in
  if next_head = game.food then
    let new_snake = next_head :: game.snake in
    match find_free_cell game.width game.height new_snake with
    | Some new_food ->
        {
          game with
          direction;
          snake = new_snake;
          food = new_food;
          round = next_round;
          commands = tl_sf game.commands;
        }
    | None -> { game with state = GameOver; round = next_round }
  else if List.exists (fun c -> c = next_head) game.snake then
    { game with state = GameOver; round = next_round }
  else
    let new_snake = drop_last (next_head :: game.snake) in
    {
      game with
      direction;
      snake = new_snake;
      round = next_round;
      commands = tl_sf game.commands;
    }

let get_state game = game.state
let get_round game = game.round
let get_points game = List.length game.snake
