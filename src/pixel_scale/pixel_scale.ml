exception PixelScalesNotEqualEnough

type t = { xstep : float; ystep : float }

let xstep pixel_scale = pixel_scale.xstep
let ystep pixel_scale = pixel_scale.ystep
let pixel_scale xstep ystep = { xstep; ystep }

(*
   Credit for rest of this file goes to mdales and his yirgacheffe library
*)

let minimal_degree_of_interest_metres = 1.0
let distance_per_degree_at_equator = 40075017.0 /. 360.0

let minimal_degree_of_interest =
  minimal_degree_of_interest_metres /. distance_per_degree_at_equator

let pp_pixel_scale pixel_scale =
  Eio.traceln "Pixel Scale: xstep: %f ystep: %f" pixel_scale.xstep
    pixel_scale.ystep

let round_up_pixels value step =
  let floored = Float.floor value in
  let diff = value -. floored in
  let degrees_diff = diff *. step in
  if degrees_diff < minimal_degree_of_interest then floored
  else Float.ceil value

let round_down_pixels value step =
  let ceiled = Float.ceil value in
  let diff = ceiled -. value in
  let degrees_diff = diff *. step in
  if degrees_diff < minimal_degree_of_interest then ceiled
  else Float.floor value

let are_two_pixel_scales_equal pixel_scale1 pixel_scale2 =
  if
    Float.abs (pixel_scale1.xstep -. pixel_scale2.xstep)
    > minimal_degree_of_interest
    || Float.abs (pixel_scale1.ystep -. pixel_scale2.ystep)
       > minimal_degree_of_interest
  then false
  else true

let rec are_pixel_scales_equal_enough_helper pixel_scale_list pixel_scale =
  match pixel_scale_list with
  | [] -> true
  | pixel_scale2 :: ps ->
      are_two_pixel_scales_equal pixel_scale pixel_scale2
      && are_pixel_scales_equal_enough_helper ps pixel_scale

let are_pixel_scales_equal_enough pixel_scale_list =
  match pixel_scale_list with
  | [] -> true
  | x :: xs -> are_pixel_scales_equal_enough_helper xs x
