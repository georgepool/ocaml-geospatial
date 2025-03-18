open Alcotest
open Area

let test_area_creation () = 
  let a = area 0.0 1.0 2.0 0.0 in 
  check (float 0.0001) "Left is 0.0" 0.0 (left a);
  check (float 0.0001) "Top is 1.0" 1.0 (top a);
  check (float 0.0001) "Right is 2.0" 2.0 (right a);
  check (float 0.0001) "Bottom is 0.0" 0.0 (bottom a)


let test_copy_area () = 
  let a1 = area 0.0 1.0 2.0 0.0 in 
  let a2 = copy_area a1 in
  check (float 0.0001) "Copied Left is 0.0" 0.0 (left a2);
  check (float 0.0001) "Copied Top is 1.0" 1.0 (top a2);
  check (float 0.0001) "Copied Right is 2.0" 2.0 (right a2);
  check (float 0.0001) "Copied Bottom is 0.0" 0.0 (bottom a2)

let test_intersection_of_same_areas () = 
  let a1 = area 0.0 1.0 2.0 0.0 in  
  let a2 = area 0.0 1.0 2.0 0.0 in 
  let ai = intersect_areas a1 a2 in
  check (float 0.0001) "Same intersection Left is 0.0" 0.0 (left ai);
  check (float 0.0001) "Same intersection Top is 1.0" 1.0 (top ai);
  check (float 0.0001) "Same intersection Right is 2.0" 2.0 (right ai);
  check (float 0.0001) "Same intersection Bottom is 0.0" 0.0 (bottom ai)

let test_intersection_areas () = 
  let a1 = area 0.0 1.0 2.0 0.0 in 
  let a2 = area 0.5 1.5 1.5 0.5 in 
  let ai = intersect_areas a1 a2 in 
  check (float 0.0001) "Intersection Left is 0.5" 0.5 (left ai);
  check (float 0.0001) "Intersection Top is 1.0" 1.0 (top ai);
  check (float 0.0001) "Intersection Right is 2.0" 1.5 (right ai);
  check (float 0.0001) "Intersection Bottom is 0.0" 0.5 (bottom ai)

let test_no_intersect_areas () =
  let a1 = area 0.0 1.0 2.0 0.0 in 
  let a2 = area 3.0 4.0 5.0 3.0 in
  check_raises "No intersection raises exception"
    (IntersectionDoesNotExist "There is no intersection here")
    (fun () -> ignore (intersect_areas a1 a2))

let test_union_areas () =
  let a1 = area 0.0 10.0 5.0 0.0 in
  let a2 = area 2.0 8.0 6.0 2.0 in
  let expected = area 0.0 10.0 6.0 0.0 in
  let result = union_areas a1 a2 in
  check (float 0.0001) "Left" (left expected) (left result);
  check (float 0.0001) "Top" (top expected) (top result);
  check (float 0.0001) "Right" (right expected) (right result);
  check (float 0.0001) "Bottom" (bottom expected) (bottom result)

let test_find_intersection_areas () =
  let areas = [
    area 0.0 10.0 10.0 0.0;
    area 2.0 8.0 8.0 2.0;
    area 3.0 7.0 7.0 3.0
  ] in
  let expected = area 3.0 7.0 7.0 3.0 in
  let result = find_intersection_areas areas in
  check (float 0.0001) "Left" (left expected) (left result);
  check (float 0.0001) "Top" (top expected) (top result);
  check (float 0.0001) "Right" (right expected) (right result);
  check (float 0.0001) "Bottom" (bottom expected) (bottom result)

let test_no_find_intersection_areas () =
  let areas = [
    area 0.0 5.0 2.0 0.0;
    area 3.0 10.0 5.0 7.0
  ] in
  check_raises "No intersection in list should raise exception"
    (IntersectionDoesNotExist "There is no intersection here")
    (fun () -> ignore (find_intersection_areas areas))

let test_find_union_areas () =
  let areas = [
    area 0.0 10.0 10.0 0.0;
    area 2.0 8.0 8.0 2.0;
    area 3.0 7.0 7.0 3.0
  ] in
  let expected = area 0.0 10.0 10.0 0.0 in
  let result = find_union_areas areas in
  check (float 0.0001) "Left" (left expected) (left result);
  check (float 0.0001) "Top" (top expected) (top result);
  check (float 0.0001) "Right" (right expected) (right result);
  check (float 0.0001) "Bottom" (bottom expected) (bottom result)

let test_no_find_union_areas () =
  check_raises "Empty list should raise exception"
    (UnionDoesNotExist "No union of list of 0 areas")
    (fun () -> ignore (find_union_areas []))

let () =
  run "Area Tests"
    [
      ("area creation", [ test_case "Create area" `Quick test_area_creation ]);
      ("copy area", [ test_case "Copy area" `Quick test_copy_area ]);
      ("intersect areas", [ test_case "Intersection of same areas" `Quick test_intersection_of_same_areas;
                            test_case "Find intersection" `Quick test_intersection_areas;
                            test_case "No intersection" `Quick test_no_intersect_areas ]);
      ("union areas", [ test_case "Find union" `Quick test_union_areas]);
      ("find intersections", [ test_case "Find intersection in list" `Quick test_find_intersection_areas;
                               test_case "No intersection in list" `Quick test_no_find_intersection_areas ]);
      ("find unions", [ test_case "Find union in list" `Quick test_find_union_areas;
                        test_case "No union in list" `Quick test_no_find_union_areas ]);
    ]