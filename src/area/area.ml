exception IntersectionDoesNotExist of string

type t = { left : float; top : float; right : float; bottom : float }

let left t = t.left
let top t = t.top
let right t = t.right
let bottom t = t.bottom
let area left top right bottom = { left; top; right; bottom }

let copy_area area =
  { left = area.left; top = area.top; right = area.right; bottom = area.bottom }

let pp_area area =
  Eio.traceln "Area: left: %f top: %f right: %f bottom: %f" area.left area.top
    area.right area.bottom

let intersect_areas a1 a2 =
  let left = max a1.left a2.left in
  let top = max a1.top a2.top in
  let right = min a1.right a2.right in
  let bottom = min a1.bottom a2.bottom in
  if left > right || top < bottom then
    raise (IntersectionDoesNotExist "There is no intersection here")
  else { left; top; right; bottom }

let rec find_intersection_areas_helper area_list area_acc =
  match area_list with
  | [] -> area_acc
  | w :: ws -> find_intersection_areas_helper ws (intersect_areas area_acc w)

let find_intersection_areas area_list =
  match area_list with
  | [] -> raise (IntersectionDoesNotExist "No intersection of list of 0 areas")
  | w :: ws -> find_intersection_areas_helper ws w
