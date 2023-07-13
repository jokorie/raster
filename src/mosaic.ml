open Core

(* You need to modify this function to blur the input image based on the
   provided radius instead of ignoring it. *)
let create_region_from_index
  ~image
  ~region_x_index
  ~region_y_index
  ~region_height
  ~region_width
  =
  Image.slice
    image
    ~x_start:(region_x_index * region_width)
    ~x_end:((region_x_index * region_width) + (region_width - 1))
    ~y_start:(region_y_index * region_height)
    ~y_end:((region_y_index * region_height) + (region_height - 1))
;;

let calculate_region_strength_diff ~thresh_region ~compare_region =
  Image.foldi thresh_region ~init:0. ~f:(fun ~x ~y acc (t_r, t_g, t_b) ->
    let c_r, c_g, c_b = Image.get compare_region ~x ~y in
    acc
    +. Float.of_int
         (((t_r - c_r) * (t_r - c_r))
          + ((t_g - c_g) * (t_g - c_g))
          + ((t_b - c_b) * (t_b - c_b)))
    |> sqrt)
;;

let swap_regions
  ~image
  ~r1
  ~r1_x_index
  ~r1_y_index
  ~r2
  ~r2_x_index
  ~r2_y_index
  ~width
  ~height
  =
  let _ =
    Image.mapi r1 ~f:(fun ~x ~y pixel1 ->
      let pixel2 = Image.get r2 ~x ~y in
      Image.set
        image
        ~x:(x + (r1_x_index * width))
        ~y:(y + (r1_y_index * height))
        pixel2;
      Image.set
        image
        ~x:(x + (r2_x_index * width))
        ~y:(y + (r2_y_index * height))
        pixel1;
      Pixel.zero)
  in
  ()
;;

let transform image ~moves ~width ~height =
  let max_region_x_index = Image.width image / width in
  let max_region_y_index = Image.height image / height in
  let mosaic_image = Blur.transform image ~radius:2 in
  (* maybe copy the image?*)
  let rec mosaic ~moves ~image =
    match moves with
    | 0 -> image
    | _ ->
      let rand_x = Random.int max_region_x_index in
      let rand_y = Random.int max_region_y_index in
      let thresh_region =
        create_region_from_index
          ~image
          ~region_x_index:rand_x
          ~region_y_index:rand_y
          ~region_height:height
          ~region_width:width
      in
      let regions_list =
        List.init max_region_y_index ~f:(fun y_index ->
          List.init max_region_x_index ~f:(fun x_index ->
            if x_index = rand_x && y_index = rand_y
            then (rand_x, rand_y), thresh_region, Float.infinity
            else (
              let compare_region =
                create_region_from_index
                  ~image
                  ~region_x_index:x_index
                  ~region_y_index:y_index
                  ~region_height:height
                  ~region_width:width
              in
              ( (x_index, y_index)
              , compare_region
              , calculate_region_strength_diff ~thresh_region ~compare_region
              ))))
      in
      let flat_regions = List.concat regions_list in
      let (b_r_x, b_r_y), best_region, _ =
        List.fold
          flat_regions
          ~init:((rand_x, rand_y), thresh_region, Float.infinity)
          ~f:(fun ((b_r_x, b_r_y), best_region, best_eval)
                  ((c_r_x, c_r_y), curr_reg, curr_eval) ->
            if Float.( < ) curr_eval best_eval
            then (c_r_x, c_r_y), curr_reg, curr_eval
            else (b_r_x, b_r_y), best_region, best_eval)
      in
      swap_regions
        ~image
        ~r1:thresh_region
        ~r1_x_index:rand_x
        ~r1_y_index:rand_y
        ~r2:best_region
        ~r2_x_index:b_r_x
        ~r2_y_index:b_r_y
        ~width
        ~height;
      mosaic ~moves:(moves - 1) ~image
  in
  mosaic ~image:mosaic_image ~moves
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
        let image =
          transform
            (Image.load_ppm ~filename)
            ~moves:20000
            ~width:5
            ~height:5
        in
        Image.save_ppm image ~filename:"mosaic.ppm"]
;;
