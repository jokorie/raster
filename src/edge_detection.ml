open! Core

(* This should look familiar by now! *)

let in_bounds ~image ~x ~y =
  if x >= 0 && x < Image.width image && y >= 0 && y < Image.height image
  then true
  else false
;;

let grad_x_kernel =
  (* row, col*)
  [ -1, -1, -1
  ; -1, 0, 0
  ; -1, 1, 1
  ; 0, -1, -2
  ; 0, 0, 0
  ; 0, 1, 2
  ; 1, -1, -1
  ; 1, 0, 0
  ; 1, 1, 1
  ]
;;

let apply_kernel ~image ~x ~y ~kernel =
  List.fold ~init:0 kernel ~f:(fun acc (y_delta, x_delta, kernel_val) ->
    let new_x = x + x_delta in
    let new_y = y + y_delta in
    match in_bounds ~image ~x:new_x ~y:new_y with
    | true ->
      let existing_red, _, _ = Image.get image ~x:new_x ~y:new_y in
      acc + (existing_red * kernel_val)
    | false -> acc)
;;

let transform image =
  let threshold_percent = 0.2 in
  let gray_image = Blur.transform image ~radius:2 |> Grayscale.transform in
  let max_val = Image.max_val image in
  let threshold = threshold_percent *. Float.of_int max_val in
  let edge_image =
    Image.mapi gray_image ~f:(fun ~x ~y (_, _, _) ->
      let grad_x =
        Float.of_int
          (apply_kernel ~image:gray_image ~x ~y ~kernel:grad_x_kernel)
      in
      let grad_y =
        Float.of_int
          (apply_kernel ~image:gray_image ~x:(-y) ~y:x ~kernel:grad_x_kernel)
      in
      let grad = sqrt ((grad_x *. grad_x) +. (grad_y *. grad_y)) in
      if Float.( >= ) grad threshold
      then Pixel.of_int max_val
      else Pixel.zero)
  in
  edge_image
;;

let command =
  Command.basic
    ~summary:"Highlight the edge of an imaage"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_edge.ppm")]
;;
