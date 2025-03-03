open Alcotest
open Window

let test_window_creation () = 
  let w = window 1 2 3 4 in 
  check int "Xoffset is 1" 1 (xoffset w);
  check int "Yoffset is 2" 2 (yoffset w);
  check int "Xsize is 3" 3 (xsize w);
  check int "Ysize is 4" 4 (ysize w)

let test_copy_window () = 
  let w1 = window 1 2 3 4 in
  let w2 = copy_window w1 in
  check int "Copied window Xoffset is 1" 1 (xoffset w2);
  check int "Copied window Yoffset is 2" 2 (yoffset w2);
  check int "Copied window Xsize is 3" 3 (xsize w2);
  check int "Copied window Ysize is 4" 4 (ysize w2)

let test_windows_are_equal_size () =
  let w1 = window 0 0 2 2 in 
  let w2 = window 0 0 1 1 in 
  let w3 = window 0 0 1 2 in 
  let w4 = window 0 0 2 1 in 
  let w5 = window 5 5 2 2 in 
  check bool "w1 and w2 not equal" false (windows_are_equal_size w1 w2);
  check bool "w1 and w3 not equal" false (windows_are_equal_size w1 w3);
  check bool "w1 and w4 not equal" false (windows_are_equal_size w1 w4);
  check bool "w1 and w5 equal" true (windows_are_equal_size w1 w5)

let () =
  run "Window Tests"
  [
    ("Window creation", [ test_case "Create window" `Quick test_window_creation ]);
    ("Copy window", [ test_case "Copy window" `Quick test_copy_window ]);
    ("Window size comparison", [ test_case "Compare window sizes" `Quick test_windows_are_equal_size ]);
  ]