open Alcotest
open Pixel_scale

let test_pixel_scale_creation () = 
  let a = pixel_scale 1.0 2.0 in 
  check (float 0.0001) "Xstep is 1.0" 1.0 (xstep a);
  check (float 0.0001) "Ystep is 2.0" 2.0 (ystep a)

let test_round_up_pixels () =
  check (float 0.0001) "Round up" 8033.0 (round_up_pixels 8033.000000000001 0.0008983152841195215);
  check (float 0.0001) "Round up" 8033.0 (round_up_pixels 8033.001 0.0008983152841195215);
  check (float 0.0001) "Round up" 8034.0 (round_up_pixels 8033.01 0.0008983152841195215);
  check (float 0.0001) "Round up" 8033.0 (round_up_pixels 8032.999999999999 0.0008983152841195215)

let test_round_down_pixels () =
  check (float 0.0001) "Round down" 
    56.0 (round_down_pixels 55.99999999999926 0.0008983152841195215);

  check (float 0.0001) "Round down" 
    56.0 (round_down_pixels 55.998 0.0008983152841195215);

  check (float 0.0001) "Round down" 
    55.0 (round_down_pixels 55.98 0.0008983152841195215);

  check (float 0.0001) "Round down" 
    55.0 (round_down_pixels 55.000000000001 0.0008983152841195215)

let () =
run "Pixel Scale Tests"
  [
    ("Pixel scale creation", [ test_case "Create pixel scale" `Quick test_pixel_scale_creation ]);
    ("Round up pixels", [ test_case "Test rounding up" `Quick test_round_up_pixels ]);
    ("Round down pixels", [ test_case "Test rounding down" `Quick test_round_down_pixels ]);
    (* ("Pixel scales equal enough", [ test_case "Compare list of pixel scales" `Quick test_are_pixel_scales_equal_enough ]); *)
  ]