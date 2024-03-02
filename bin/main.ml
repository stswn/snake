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
  let game = G.init ~width:40 ~height:20 |> G.start in
  Format.printf "Step 1: %a\n" G.pp game;
  let game2 = G.turn_left game in
  Format.printf "Before step 2: %a\n" G.pp game2;
  let game3 = G.advance game2 in
  Format.printf "After step 2: %a\n" G.pp game3
