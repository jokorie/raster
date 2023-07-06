open Core

(* This should look familiar by now! *)

let in_bounds ~image ~x ~y =
  if x >= 0 && x < Image.width image && y >= 0 && y < Image.height image
  then true
  else false
;;

let coord_dir =
  [ 1, 0, 7. /. 16.; -1, 1, 3. /. 16.; 0, 1, 5. /. 16.; 1, 1, 1. /. 16. ]
;;

let transform image =
  let gray_image = Grayscale.transform image in
  let _ =
    Image.mapi gray_image ~f:(fun ~x ~y (r, _, _) ->
      let max_val = Image.max_val gray_image in
      let difference = r - (max_val / 2) in
      let calculated_error =
        if difference <= 0 then r else r - max_val
        (* this should be old - new *)
      in
      let () =
        List.iter coord_dir ~f:(fun (x_delta, y_delta, error_weight) ->
          let new_x = x + x_delta in
          let new_y = y + y_delta in
          match in_bounds ~image:gray_image ~x:new_x ~y:new_y with
          | true ->
            let existing_red, _, _ =
              Image.get gray_image ~x:new_x ~y:new_y
            in
            let new_pixel_value =
              Int.of_float (Float.of_int calculated_error *. error_weight)
              + existing_red
            in
            Image.set
              gray_image
              ~x:new_x
              ~y:new_y
              (new_pixel_value, new_pixel_value, new_pixel_value)
          | false -> ())
      in
      Image.set
        gray_image
        ~x
        ~y
        (if difference > 0 then max_val, max_val, max_val else 0, 0, 0);
      0, 0, 0)
  in
  gray_image
;;

let command =
  Command.basic
    ~summary:"Dither an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
