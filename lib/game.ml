type state = Waiting | Started | Paused | GameOver [@@deriving show]
type cell = { x : int; y : int } [@@deriving show]
type direction = Left | Up | Right | Down [@@deriving show, enum]
type snake = cell list [@@deriving show]

type t = {
  width : int;
  height : int;
  state : state;
  round : int;
  current_direction : direction;
  next_direction : direction;
  snake : cell list;
  food : cell;
}
[@@deriving show]

(* Finds free cell to put food  or initial snake in *)
let find_free_cell width height occupied =
  let free_cells = (width * height) - List.length occupied in
  let start_idx = Random.int free_cells in
  let rec find_cell cell idx =
    let cell_occupied = List.exists (fun c -> c = cell) occupied in
    match idx with
    | 0 -> if cell_occupied then Option.none else Option.some cell
    | _ ->
        if cell.x < width - 1 then
          find_cell { cell with x = cell.x + 1 } (idx - 1)
        else if cell.y < height - 1 then
          find_cell { x = 0; y = cell.y + 1 } (idx - 1)
        else find_cell { x = 0; y = 0 } (idx - 1)
  in
  find_cell { x = 0; y = 0 } start_idx

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
    current_direction = direction;
    next_direction = direction;
    snake;
    food;
  }

let start game =
  match game.state with Waiting -> { game with state = Started } | _ -> game

let when_started game updated_game =
  match game.state with Started -> updated_game | _ -> game

let turn_right game =
  when_started game
  @@
  let next_direction =
    (direction_to_enum game.current_direction + 1) mod 4
    |> direction_of_enum |> Option.get
  in
  { game with next_direction }

let turn_left game =
  when_started game
  @@
  let next_direction =
    (direction_to_enum game.current_direction + 5) mod 4
    |> direction_of_enum |> Option.get
  in
  { game with next_direction }

let set_paused game paused =
  match (game, paused) with
  | ({ state = Started; _ } as s), true -> { s with state = Paused }
  | ({ state = Paused; _ } as s), false -> { s with state = Started }
  | s, _ -> s

let get_paused game = game.state = Paused
let get_snake game = game.snake
let get_food game = game.food
let nmod x y = ((x mod y) + y) mod y

let drop_last snake =
  let rec drop_last_reversed s acc =
    match (s, acc) with
    | [], acc -> acc
    | _ :: [], acc -> acc
    | x :: xs, acc -> drop_last_reversed xs (x :: acc)
  in
  drop_last_reversed snake [] |> List.rev

let advance game =
  when_started game
  @@
  let current_head = List.hd game.snake in
  let next_head =
    match game.next_direction with
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
          snake = new_snake;
          food = new_food;
          current_direction = game.next_direction;
          round = next_round;
        }
    | None -> { game with state = GameOver; round = next_round }
  else if List.exists (fun c -> c = next_head) game.snake then
    { game with state = GameOver; round = next_round }
  else
    let new_snake = drop_last (next_head :: game.snake) in
    {
      game with
      snake = new_snake;
      current_direction = game.next_direction;
      round = next_round;
    }

let get_round game = game.round
let get_points game = List.length game.snake - 1
