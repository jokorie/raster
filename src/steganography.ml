open Core

(* You need to change the implementation of this function so that it does
   something to the image instead of just leaving it untouched. *)
let transform image : Image.t =
  let old_pixels =
    Image.map image ~f:(fun (r, g, b) ->
      let new_r = r % 4 * 64 in
      let new_g = g % 4 * 64 in
      let new_b = b % 4 * 64 in
      new_r, new_g, new_b)
  in
  old_pixels
;;

let command =
  Command.basic
    ~summary:"Create an image through steganography"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm image ~filename:"mystery.ppm"]
;;
(* (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_gray.ppm")] *)
