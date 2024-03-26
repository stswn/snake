module R = Raylib
module G = Snake.Game

let board_width = 40
let board_height = 30
let field_size = 40
let status_x = 60
let status_y = 60
let turn_length = 0.2

let setup () =
  R.init_window (board_width * field_size) (board_height * field_size) "Snake"

let draw_board board =
  board
  |> Array.iteri (fun x row ->
         row
         |> Array.iteri (fun y field ->
                let pos_x = x * field_size in
                let pos_y = y * field_size in
                let color =
                  match field with
                  | G.Empty -> R.Color.skyblue
                  | G.Food -> R.Color.green
                  | G.Snake -> R.Color.orange
                in
                R.draw_rectangle (pos_x + 1) (pos_y + 1) (field_size - 2)
                  (field_size - 2) color))

let show_text t = R.draw_text t status_x status_y field_size R.Color.darkblue

let display_state game =
  match game |> G.get_state with
  | G.Waiting -> show_text "Press arrow up to start"
  | G.Paused -> show_text "Paused: press 'P' to resume"
  | G.GameOver ->
      show_text
        ("Game Over: " ^ (game |> G.get_points |> string_of_int) ^ " points")
  | G.Started -> show_text (game |> G.get_points |> string_of_int)

let draw game =
  R.begin_drawing ();
  R.clear_background R.Color.beige;
  game |> G.get_board |> draw_board;
  game |> display_state;
  R.draw_text
    ((R.get_fps () |> string_of_int) ^ " FPS")
    status_x
    ((board_height * field_size) - status_y)
    field_size R.Color.darkblue;
  R.end_drawing ()

let handle_input game =
  if R.is_key_released R.Key.Up then game |> G.start
  else if R.is_key_released R.Key.Left then game |> G.turn_left
  else if R.is_key_released R.Key.Right then game |> G.turn_right
  else if R.is_key_released R.Key.P then
    game |> G.set_paused (not (game |> G.get_paused))
  else game

let rec loop game elapsed =
  if R.window_should_close () then R.close_window () else draw game;
  let game = handle_input game in
  let new_elapsed = elapsed +. R.get_frame_time () in
  if new_elapsed >= turn_length then
    let game = game |> G.advance in
    loop game (turn_length -. new_elapsed)
  else loop game new_elapsed

let () =
  Random.self_init ();
  let game = G.init ~width:board_width ~height:board_height in
  setup ();
  loop game 0.0
