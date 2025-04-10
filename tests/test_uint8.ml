open Layer

let test_uint8_tiff file_name () = 
  Eio_main.run @@ fun env ->
    let fs = Eio.Stdenv.fs env in 
    let path = Eio.Path.(fs / file_name) in 
    let layer = UInt8Layer.layer_from_file path in

    let empty_layer = UInt8Layer.empty_layer_like layer in 

    let empty_layer_total = UInt8OperationLayer.sum_layer (UInt8OperationLayer.SingleLayer empty_layer) in

    Alcotest.(check int) ("Sum of empty layer for " ^ file_name) 0 empty_layer_total


let file_names = [ "files/cea.tiff"; 
(* "files/uniform.tiff" *)
]

let tests = 
  List.map 
    (fun file_name -> 
      (* Unique test name for each file *)
      ("Test for " ^ file_name, `Quick, test_uint8_tiff file_name))
    file_names

let () = 
  Alcotest.run "UInt8Layer Tests" [ "Empty Layer Sum", tests ]