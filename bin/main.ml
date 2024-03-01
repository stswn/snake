(* module R = Raylib *)
module G = Snake.Game

(* let setup () =
  R.init_window 1600 900 "raylib [core] example - basic window";
  R.set_target_fps 60

let rec loop () =
  match R.window_should_close () with
  | true -> R.close_window ()
  | false ->
      let open R in
      begin_drawing ();
      clear_background Color.raywhite;
      draw_text "Congrats! You created your first window!" 380 400 40
        Color.lightgray;
      end_drawing ();
      loop ()
*)
(* let () = setup () |> loop *)

let () = 
  Random.self_init ();
  let game = G.init ~width:40 ~height:20 in
  let game2 = G.advance (G.start game) in
  Format.printf "Game: %a" G.pp game2;
