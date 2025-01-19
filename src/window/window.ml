exception WindowsAreNotSameSize;;

type t = {
  xoffset: int;
  yoffset: int;
  xsize: int;
  ysize: int;
}

let xoffset window = window.xoffset

let yoffset window = window.yoffset

let xsize window = window.xsize

let ysize window = window.ysize

let copy_window window = {
  xoffset = window.xoffset;
  yoffset = window.yoffset;
  xsize = window.xsize;
  ysize = window.ysize
}

let pp_window window = 
  Eio.traceln "Window: xoffset: %i yoffset: %i xsize: %i ysize: %i" window.xoffset window.yoffset window.xsize window.ysize
  
let windows_are_equal window1 window2 = 
  (window1.xoffset = window2.xoffset) && 
  (window1.yoffset = window2.yoffset) &&
  (window1.xsize = window2.xsize) &&
  (window1.ysize = window2.ysize) 
