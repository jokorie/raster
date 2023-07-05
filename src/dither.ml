open Core

(* This should look familiar by now! *)

let in_bounds ~image ~x ~y =
  if x > 0 && x< Image.width image -1 
    && y > 0 && y < Image.height image - 1 
  then true
  else false
;;

let transform image =
  let gray_image = Grayscale.transform image in 
  Image.mapi gray_image ~f:( fun ~x ~y (r,g,b) -> 
    let difference = r - Image.max_val gray_image /2 in
     if in_bounds ~image:gray_image ~x:x+1 ~y then 
      () 
    else ()
    ) 
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
