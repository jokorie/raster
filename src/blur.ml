open Core

(* You need to modify this function to blur the input image based on the
   provided radius instead of ignoring it. *)
let transform image ~radius =
  let blurred_image = Image.copy image in
  let _ =
    Array.init (Image.width image) ~f:(fun x ->
      Array.init (Image.height image) ~f:(fun y ->
        let x_start = if x - radius < 0 then 0 else x - radius in
        let x_end =
          if x + radius > Image.width image - 1
          then Image.width image
          else x + radius
        in
        let y_start = if y - radius < 0 then 0 else y - radius in
        let y_end =
          if y + radius > Image.height image - 1
          then Image.height image
          else y + radius
        in
        let slice = Image.slice image ~x_start ~x_end ~y_start ~y_end in
        let avg = Image.mean_pixel slice in
        Image.set blurred_image ~x ~y avg))
  in
  blurred_image
;;

let command =
  Command.basic
    ~summary:"Blur an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the radius to use when blurring (higher = more blurred)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~radius in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;
