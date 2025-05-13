(lang dune 3.9)
(name ocaml_geospatial)
(generate_opam_files true)


(package
 (name ocaml_geospatial)
 (synopsis "Pure OCaml Geospatial Library")
 (description "This library allows for geospatial programming on GeoTIFF data.")
 (depends
   tiff window pixel_scale area eio eio_main owl cstruct fmt optint))
