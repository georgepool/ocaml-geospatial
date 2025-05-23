module File = struct
  type ro = file_offset:Optint.Int63.t -> Cstruct.t list -> unit
  (** Read-only access to a file that supports reading at a particular offset.
      The read should be exact and raise [End_of_file] should that be the case.
  *)
end

type header = { kind : kind; byte_order : endianness; offset : Optint.Int63.t }
and kind = Tiff | Bigtiff
and endianness = Big | Little

module Endian = struct
  let uint16 ?(offset = 0) endian buf =
    match endian with
    | Big -> Cstruct.BE.get_uint16 buf offset
    | Little -> Cstruct.LE.get_uint16 buf offset

  let uint32 ?(offset = 0) endian buf =
    match endian with
    | Big -> Cstruct.BE.get_uint32 buf offset
    | Little -> Cstruct.LE.get_uint32 buf offset

  let uint64 ?(offset = 0) endian buf =
    match endian with
    | Big -> Cstruct.BE.get_uint64 buf offset
    | Little -> Cstruct.LE.get_uint64 buf offset

  let double ?(offset = 0) endian buf =
    Int64.float_of_bits (uint64 ~offset endian buf)
end

let header ro =
  (* We may get more bytes than we want, but this is to support Bigtiffs *)
  let buf = Cstruct.create 16 in
  ro ~file_offset:Optint.Int63.zero [ buf ];
  let byte_order =
    match Cstruct.to_string ~off:0 ~len:2 buf with
    | "II" -> Little
    | "MM" -> Big
    | e -> failwith ("Unknown endianness of TIFF file " ^ e)
  in
  let magic = Endian.uint16 ~offset:2 byte_order buf in
  let kind, offset =
    match magic with
    | 42 ->
        (Tiff, Endian.uint32 ~offset:4 byte_order buf |> Optint.Int63.of_int32)
    | 43 ->
        ( Bigtiff,
          Endian.uint64 ~offset:8 byte_order buf |> Optint.Int63.of_int64 )
    | i -> failwith ("Unknown magic number: " ^ string_of_int i)
  in
  { byte_order; kind; offset }

module Ifd = struct
  type field =
    | Byte
    | Ascii
    | Short
    | Long
    | Rational
    | Sbyte
    | Undefined
    | Sshort
    | Slong
    | Srational
    | Float
    | Double
    | Long8
    | Slong8
    | Ifd8

  let field_of_int = function
    | 1 -> Byte
    | 2 -> Ascii
    | 3 -> Short
    | 4 -> Long
    | 5 -> Rational
    | 6 -> Sbyte
    | 7 -> Undefined
    | 8 -> Sshort
    | 9 -> Slong
    | 10 -> Srational
    | 11 -> Float
    | 12 -> Double
    | 16 -> Long8
    | 17 -> Slong8
    | 18 -> Ifd8
    | n -> invalid_arg ("Field kind " ^ string_of_int n)

  let pp_field ppf = function
    | Byte -> Fmt.pf ppf "byte"
    | Ascii -> Fmt.pf ppf "ascii"
    | Short -> Fmt.pf ppf "short"
    | Long -> Fmt.pf ppf "long"
    | Rational -> Fmt.pf ppf "rational"
    | Sbyte -> Fmt.pf ppf "sbyte"
    | Undefined -> Fmt.pf ppf "undefined"
    | Sshort -> Fmt.pf ppf "sshort"
    | Slong -> Fmt.pf ppf "slong"
    | Srational -> Fmt.pf ppf "srational"
    | Float -> Fmt.pf ppf "float"
    | Double -> Fmt.pf ppf "double"
    | Long8 -> Fmt.pf ppf "long8"
    | Slong8 -> Fmt.pf ppf "slong8"
    | Ifd8 -> Fmt.pf ppf "ifd8"

  let rec field_byte_size (x : field) =
    match x with
    | Byte -> 1
    | Ascii -> 1
    | Short -> 2
    | Long -> 4
    | Rational -> field_byte_size Long * 2
    | Sbyte -> 1
    | Undefined -> 1
    | Sshort -> 2
    | Slong -> 4
    | Srational -> field_byte_size Slong * 2
    | Float -> 4
    | Double -> 8
    | Long8 -> 8
    | Slong8 -> 8
    | Ifd8 -> 8

  type tag =
    | ImageWidth
    | ImageLength
    | BitsPerSample
    | Compression
    | PhotometricInterpretation
    | StripOffsets
    | RowsPerStrip
    | StripByteCounts
    | XResolution
    | YResolution
    | PlanarConfiguration
    | ResolutionUnit
    | SampleFormat
    | SamplesPerPixel
    | ModelPixelScale
    | ModelTiepoint
    | GeoDoubleParams
    | GeoAsciiParams
    | GeoKeyDirectory
    | Predictor
    | TileWidth
    | TileHeight
    | TileOffsets
    | TileByteCounts
    | GdalMetadata
    | Unknown of int

  let tag_of_int = function
    | 256 -> ImageWidth
    | 257 -> ImageLength
    | 258 -> BitsPerSample
    | 259 -> Compression
    | 262 -> PhotometricInterpretation
    | 273 -> StripOffsets
    | 277 -> SamplesPerPixel
    | 278 -> RowsPerStrip
    | 279 -> StripByteCounts
    | 282 -> XResolution
    | 283 -> YResolution
    | 284 -> PlanarConfiguration
    | 296 -> ResolutionUnit
    | 339 -> SampleFormat
    | 33550 -> ModelPixelScale
    | 33922 -> ModelTiepoint
    | 34736 -> GeoDoubleParams
    | 34737 -> GeoAsciiParams
    | 34735 -> GeoKeyDirectory
    | 317 -> Predictor
    | 322 -> TileWidth
    | 323 -> TileHeight
    | 324 -> TileOffsets
    | 325 -> TileByteCounts
    | 42112 -> GdalMetadata
    | i -> Unknown i

  let pp_tag ppf (x : tag) =
    match x with
    | ImageWidth -> Fmt.string ppf "image-width"
    | ImageLength -> Fmt.string ppf "image-length"
    | BitsPerSample -> Fmt.string ppf "bits-per-sample"
    | Compression -> Fmt.string ppf "compression"
    | PhotometricInterpretation -> Fmt.string ppf "photometric-interpretation"
    | StripOffsets -> Fmt.string ppf "strip-offsets"
    | RowsPerStrip -> Fmt.string ppf "rows-per-strip"
    | StripByteCounts -> Fmt.string ppf "strip-byte-counts"
    | XResolution -> Fmt.string ppf "x-resolution"
    | YResolution -> Fmt.string ppf "y-resolution"
    | ResolutionUnit -> Fmt.string ppf "resolution-unit"
    | PlanarConfiguration -> Fmt.string ppf "planar-configuration"
    | SampleFormat -> Fmt.string ppf "sample-format"
    | SamplesPerPixel -> Fmt.string ppf "samples-per-pixel"
    | ModelPixelScale -> Fmt.string ppf "model-pixel-scale"
    | ModelTiepoint -> Fmt.string ppf "model-tiepoint"
    | GeoDoubleParams -> Fmt.string ppf "geo-double-params"
    | GeoAsciiParams -> Fmt.string ppf "geo-ascii-params"
    | GeoKeyDirectory -> Fmt.string ppf "geo-key-directory"
    | Predictor -> Fmt.string ppf "predictor"
    | TileWidth -> Fmt.string ppf "tile-width"
    | TileHeight -> Fmt.string ppf "tile-height"
    | TileOffsets -> Fmt.string ppf "tile-offsets"
    | TileByteCounts -> Fmt.string ppf "tile-byte-counts"
    | GdalMetadata -> Fmt.string ppf "gdal-metadata"
    | Unknown i -> Fmt.pf ppf "unknown-%i" i

  type t = {
    entries : entry list;
    data_offsets : int list;
    data_bytecounts : int list;
    header : header;
    ro : File.ro;
  }

  and entry = {
    tag : tag;
    field : field;
    count : int64;
    offset : int64; (* Overallocate for normal tiffs but needed for bigtiffs *)
  }

  type compression =
    | No_compression
    | CCITTRLE
    | PACKBITS
    | CCITTFAX3
    | CCITTFAX4
    | LZW
    | OJPEG
    | JPEG
    | DEFLATE
    | ADOBE_DEFLATE
    | Other of int

  let compression_of_int = function
    | 1 -> No_compression
    | 2 -> CCITTRLE
    | 32773 -> PACKBITS
    | 3 -> CCITTFAX3
    | 4 -> CCITTFAX4
    | 5 -> LZW
    | 6 -> OJPEG
    | 7 -> JPEG
    | 32946 -> DEFLATE
    | 8 -> ADOBE_DEFLATE
    | i -> Other i

  let compression_to_int = function
    | No_compression -> 1
    | CCITTRLE -> 2
    | PACKBITS -> 32773
    | CCITTFAX3 -> 3
    | CCITTFAX4 -> 4
    | LZW -> 5
    | OJPEG -> 6
    | JPEG -> 7
    | DEFLATE -> 32946
    | ADOBE_DEFLATE -> 8
    | Other i -> i

  let entries t = t.entries
  let data_offsets t = t.data_offsets
  let data_bytecounts t = t.data_bytecounts

  let pp_entry ppf e =
    Fmt.pf ppf "tag: %a, field: %a, count: %Ld, value/offset: %Ld" pp_tag e.tag
      pp_field e.field e.count e.offset

  let lookup e tag = List.find_opt (fun e -> e.tag = tag) e
  let lookup_exn e tag = List.find (fun e -> e.tag = tag) e

  (* let read_entry e =
     match e.field with
     | Short -> e.offset |> Int64.to_int
     | _ ->
         raise
           (Invalid_argument (Fmt.str "Bad entry for short read: %a" pp_entry e)) *)

  let read_entry e =
    match e.field with
    | Short -> e.offset |> Int64.to_int
    | Long -> e.offset |> Int64.to_int
    | _ ->
        raise
          (Invalid_argument (Fmt.str "Bad entry for short read: %a" pp_entry e))

  let height e = lookup_exn e.entries ImageLength |> read_entry
  let width e = lookup_exn e.entries ImageWidth |> read_entry
  let rows_per_strip e = lookup_exn e.entries RowsPerStrip |> read_entry
  let samples_per_pixel e = lookup_exn e.entries SamplesPerPixel |> read_entry
  let add_int optint i = Optint.Int63.(add optint (of_int i))

  let is_immediate (entry : entry) =
    Int64.equal entry.count 1L
    && match entry.field with Byte | Short | Long -> true | _ -> false

  let get_dataset_offsets endian entries reader =
    match lookup entries StripOffsets with
    | None -> []
    | Some strip_offsets ->
        let strip_count = Int64.to_int strip_offsets.count in
        if is_immediate strip_offsets then [ read_entry strip_offsets ]
        else
          let strips = ref [] in
          let strip_bytes =
            match strip_offsets.field with
            | Short -> 2
            | Long -> 4
            | Long8 -> 8
            | _ ->
                Fmt.failwith "Unsupported strip length: %a" pp_field
                  strip_offsets.field
          in
          let length = strip_count * strip_bytes in
          let buf = Cstruct.create length in
          let strip_offset = Optint.Int63.of_int64 strip_offsets.offset in
          reader ~file_offset:strip_offset [ buf ];
          let get_offset ~offset buf = function
            | Short -> Endian.uint16 ~offset endian buf
            | Long -> Endian.uint32 ~offset endian buf |> Int32.to_int
            | _ -> Fmt.failwith "Unsupported"
          in
          for i = 0 to strip_count - 1 do
            strips :=
              get_offset ~offset:(i * strip_bytes) buf strip_offsets.field
              :: !strips
          done;
          List.rev !strips

  let get_bytecounts endian entries reader =
    match lookup entries StripByteCounts with
    | None -> []
    | Some strip_offsets ->
        if is_immediate strip_offsets then [ read_entry strip_offsets ]
        else
          let strip_count = Int64.to_int strip_offsets.count in
          let strips = ref [] in
          let strip_bytes =
            match strip_offsets.field with
            | Short -> 2
            | Long -> 4
            | Long8 -> 8
            | _ ->
                Fmt.failwith "Unsupported strip length: %a" pp_field
                  strip_offsets.field
          in
          let length = strip_count * strip_bytes in
          let buf = Cstruct.create length in
          let strip_offset = Optint.Int63.of_int64 strip_offsets.offset in
          reader ~file_offset:strip_offset [ buf ];
          let get_offset ~offset buf = function
            | Short -> Endian.uint16 ~offset endian buf
            | Long -> Endian.uint32 ~offset endian buf |> Int32.to_int
            | Long8 -> failwith "TODO"
            | _ ->
                Fmt.failwith "Unsupported strip length: %a" pp_field
                  strip_offsets.field
          in
          for i = 0 to strip_count - 1 do
            strips :=
              get_offset ~offset:(i * strip_bytes) buf strip_offsets.field
              :: !strips
          done;
          List.rev !strips

  let max_group lst n =
    let rec loop acc t =
      match (acc, t) with
      | [], _ -> assert false
      | _, [] -> List.rev acc
      | a :: acc, v :: vs ->
          if List.length a >= n then loop ([ v ] :: List.rev a :: acc) vs
          else loop ((v :: a) :: acc) vs
    in
    loop [ [] ] lst

  let read_entry_raw' offset field count reader =
    let file_offset = offset |> Optint.Int63.of_int64 in
    let byte_size = field_byte_size field in
    let bufs = List.init count (fun _ -> Cstruct.create byte_size) in
    let groups = if count > 256 then max_group bufs 256 else [ bufs ] in
    let run () =
      (* TODO: Common Fiber.List.map abstraction ? *)
      List.fold_left
        (fun file_offset bufs ->
          reader ~file_offset bufs;
          Optint.Int63.add file_offset
            (Optint.Int63.of_int
               (List.fold_left (fun acc buf -> Cstruct.length buf + acc) 0 bufs)))
        file_offset groups
    in
    let (_ : Optint.Int63.t) = run () in
    List.concat groups

  let read_entry_raw ?count entry =
    let count =
      match count with Some c -> c | None -> entry.count |> Int64.to_int
    in
    read_entry_raw' entry.offset entry.field count

  let pixel_scale t =
    let entry = lookup_exn t.entries ModelPixelScale in
    let scales = read_entry_raw entry t.ro in
    assert (List.length scales = 3);
    List.map (Endian.double t.header.byte_order) scales |> Array.of_list

  let bits_per_sample t =
    let entry = lookup_exn t.entries BitsPerSample in
    if entry.count = 1L then [ Int64.to_int entry.offset ]
    else
      let scales = read_entry_raw entry t.ro in
      List.map (Endian.uint16 t.header.byte_order) scales

  let planar_configuration t =
    lookup_exn t.entries PlanarConfiguration |> read_entry

  let compression t =
    lookup_exn t.entries Compression |> read_entry |> compression_of_int

  let tiepoint t =
    let entry = lookup_exn t.entries ModelTiepoint in
    let scales = read_entry_raw entry t.ro in
    assert (List.length scales mod 6 = 0);
    List.map (Endian.double t.header.byte_order) scales |> Array.of_list

  let geo_double_params t =
    let entry = lookup_exn t.entries GeoDoubleParams in
    let doubles = read_entry_raw entry t.ro in
    Array.map (Endian.double t.header.byte_order) (Array.of_list doubles)

  let string_of_int64 endianness i =
    let buf = Cstruct.create 8 in
    if endianness = Big then Cstruct.BE.set_uint64 buf 0 i;
    if endianness = Little then Cstruct.LE.set_uint64 buf 0 i;
    Cstruct.to_string buf

  let geo_ascii_params t =
    (* https://docs.ogc.org/is/19-008r4/19-008r4.html#_requirements_class_geoasciiparamstag *)
    let entry = lookup_exn t.entries GeoAsciiParams in
    let s =
      if entry.count <= 8L then string_of_int64 t.header.byte_order entry.offset
      else
        let ascii = read_entry_raw entry t.ro in
        Cstruct.concat ascii |> Cstruct.to_string
    in
    let len = String.length s in
    (* Remove the ASCII null byte *)
    let s = String.sub s 0 (len - 1) in
    String.split_on_char '|' s |> List.filter (fun v -> not (String.equal "" v))

  module GeoKeys = struct
    type ifd = t

    type key =
      | GTModelTypeGeoKey
      | GTRasterTypeGeoKey
      | GeographicTypeGeoKey
      | GeogCitationGeoKey
      | GeogAngularUnitsGeoKey
      | GeogSemiMajorAxisGeoKey
      | GeogInvFlatteningGeoKey
      | Unknown of int

    let pp_key ppf = function
      | Unknown i -> Fmt.pf ppf "unknown-%i" i
      | GTModelTypeGeoKey -> Fmt.string ppf "gt-model-type"
      | GTRasterTypeGeoKey -> Fmt.string ppf "gt-raster-type"
      | GeographicTypeGeoKey -> Fmt.string ppf "geographic-type"
      | GeogCitationGeoKey -> Fmt.string ppf "geog-citation"
      | GeogAngularUnitsGeoKey -> Fmt.string ppf "geog-angular-units"
      | GeogSemiMajorAxisGeoKey -> Fmt.string ppf "geog-semi-major-axis"
      | GeogInvFlatteningGeoKey -> Fmt.string ppf "geog-inv-flattening"

    let key_of_id = function
      | 1024 -> GTModelTypeGeoKey
      | 1025 -> GTRasterTypeGeoKey
      | 2048 -> GeographicTypeGeoKey
      | 2049 -> GeogCitationGeoKey
      | 2054 -> GeogAngularUnitsGeoKey
      | 2057 -> GeogSemiMajorAxisGeoKey
      | 2059 -> GeogInvFlatteningGeoKey
      | i -> Unknown i

    type model_type = Projected | Geographic | Geocentric | Other of int

    let model_type_of_int = function
      | 1 -> Projected
      | 2 -> Geographic
      | 3 -> Geocentric
      | i -> Other i

    type raster_type = RasterPixelIsArea | RasterPixelIsPoint | Other of int

    let raster_type_of_int = function
      | 1 -> RasterPixelIsArea
      | 2 -> RasterPixelIsPoint
      | i -> Other i

    type angular_units =
      | Radian
      | Degree
      | Arc_minute
      | Arc_second
      | Grad
      | Gon
      | DMS
      | DMS_hemisphere

    let angular_units_of_int = function
      | 9101 -> Radian
      | 9102 -> Degree
      | 9103 -> Arc_minute
      | 9104 -> Arc_second
      | 9105 -> Grad
      | 9106 -> Gon
      | 9107 -> DMS
      | 9108 -> DMS_hemisphere
      | _ -> failwith "Unknown angular units"

    let angular_units_to_string = function
      | Radian -> "radian"
      | Degree -> "degree"
      | Arc_minute -> "arc-minute"
      | Arc_second -> "arc-second"
      | Grad -> "grad"
      | Gon -> "gon"
      | DMS -> "dms"
      | DMS_hemisphere -> "dms-hemisphere"

    type entry = {
      key : key;
      field : [ `Immediate | `Loc of int ];
      count : int;
      offset : int64; (* Overallocate for normal tiffs but needed for bigtiffs *)
    }

    let pp_field ppf = function
      | `Immediate -> Fmt.pf ppf "imm"
      | `Loc i -> pp_tag ppf (tag_of_int i)

    let pp_entry ppf e =
      Fmt.pf ppf "geokey: %a, field: %a, count: %i, value/offset: %Ld" pp_key
        e.key pp_field e.field e.count e.offset

    type t = {
      version : int;
      revision : int;
      minor : int;
      geo_entries : entry list;
    }

    let entries t =
      let entry = lookup_exn t.entries GeoKeyDirectory in
      let ascii = read_entry_raw entry t.ro in
      let values = List.map (Endian.uint16 t.header.byte_order) ascii in
      match values with
      | version :: revision :: minor :: _count :: rest ->
          let rec loop acc = function
            | key :: field :: count :: voff :: more ->
                let k = key_of_id key in
                let f = if field = 0 then `Immediate else `Loc field in
                let e =
                  { key = k; field = f; count; offset = Int64.of_int voff }
                in
                loop (e :: acc) more
            | _ -> List.rev acc
          in
          let geo_entries = loop [] rest in
          { version; revision; minor; geo_entries }
      | _ -> invalid_arg "GeoKeyDirectory Malformed!"

    let get_geo_entries t = t.geo_entries

    let pp ppf t =
      Fmt.pf ppf "version: %i, revision: %i, minor: %i, count: %i\n" t.version
        t.revision t.minor
        (List.length t.geo_entries);
      Fmt.(list ~sep:Fmt.cut pp_entry) ppf t.geo_entries

    let lookup_key_exn e key = List.find (fun e -> e.key = key) e

    let model_type e =
      let key = lookup_key_exn e.geo_entries GTModelTypeGeoKey in
      key.offset |> Int64.to_int |> model_type_of_int

    let angular_units e =
      let key = lookup_key_exn e.geo_entries GeogAngularUnitsGeoKey in
      key.offset |> Int64.to_int |> angular_units_of_int

    let raster_type e =
      let key = lookup_key_exn e.geo_entries GTRasterTypeGeoKey in
      key.offset |> Int64.to_int |> raster_type_of_int

    let geo_citation t e =
      let key = lookup_key_exn e.geo_entries GeogCitationGeoKey in
      match key.field with
      | `Immediate -> assert false
      | `Loc i -> (
          match tag_of_int i with
          | GeoAsciiParams ->
              List.nth (geo_ascii_params t) (Int64.to_int key.offset)
          | _ -> "Unknown")

    let semi_major_axis t e =
      let key = lookup_key_exn e.geo_entries GeogSemiMajorAxisGeoKey in
      match key.field with
      | `Immediate -> assert false
      | `Loc i -> (
          match tag_of_int i with
          | GeoDoubleParams -> (geo_double_params t).(Int64.to_int key.offset)
          | _ -> failwith "Unknown location of semi major axis double")

    let inv_flattening t e =
      let key = lookup_key_exn e.geo_entries GeogInvFlatteningGeoKey in
      match key.field with
      | `Immediate -> assert false
      | `Loc i -> (
          match tag_of_int i with
          | GeoDoubleParams -> (geo_double_params t).(Int64.to_int key.offset)
          | _ -> failwith "Unknown location of semi major axis double")
  end

  let geo_key_directory t = GeoKeys.entries t

  let predictor t =
    let entry = lookup_exn t.entries Predictor in
    Int64.to_int entry.offset

  let tile_width t =
    let entry = lookup_exn t.entries TileWidth in
    Int64.to_int entry.offset

  let tile_height t =
    let entry = lookup_exn t.entries TileHeight in
    Int64.to_int entry.offset

  let tiles_across t =
    let twidth = tile_width t in
    (width t + twidth - 1) / twidth

  let tiles_down t =
    let theight = tile_height t in
    (height t + theight - 1) / theight

  let tile_offsets t =
    let entry = lookup_exn t.entries TileOffsets in
    let offsets = read_entry_raw entry t.ro in
    List.map (Endian.uint64 t.header.byte_order) offsets

  let tile_byte_counts t =
    let entry = lookup_exn t.entries TileByteCounts in
    let offsets = read_entry_raw entry t.ro in
    if field_byte_size entry.field = 4 then
      List.map (Endian.uint32 t.header.byte_order) offsets
      |> List.map Int64.of_int32
    else List.map (Endian.uint64 t.header.byte_order) offsets

  let v ~file_offset header reader =
    let endian = header.byte_order in
    let incr_offset = add_int file_offset in
    (* More than Tiff needs, but how much Bigtiff needs *)
    let size_buf = Cstruct.create 8 in
    let bufs = [ size_buf ] in
    reader ~file_offset bufs;
    let read, count =
      match header.kind with
      | Tiff -> (2, Endian.uint16 endian size_buf)
      | Bigtiff -> (8, Endian.uint64 endian size_buf |> Int64.to_int)
    in
    let entry_size = if header.kind = Tiff then 12 else 20 in
    let buf = Cstruct.create (entry_size * count) in
    reader ~file_offset:(incr_offset read) [ buf ];
    let entries = ref [] in
    for i = 0 to count - 1 do
      match header.kind with
      | Tiff ->
          let tag = Endian.uint16 ~offset:(i * 12) endian buf |> tag_of_int in
          let field =
            Endian.uint16 ~offset:((i * 12) + 2) endian buf |> field_of_int
          in
          let count =
            Endian.uint32 ~offset:((i * 12) + 4) endian buf |> Int64.of_int32
          in
          let offset =
            Endian.uint32 ~offset:((i * 12) + 8) endian buf |> Int64.of_int32
          in
          entries := { tag; field; count; offset } :: !entries
      | Bigtiff ->
          let tag = Endian.uint16 ~offset:(i * 20) endian buf |> tag_of_int in
          let field =
            Endian.uint16 ~offset:((i * 20) + 2) endian buf |> field_of_int
          in
          let count = Endian.uint64 ~offset:((i * 20) + 4) endian buf in
          let offset = Endian.uint64 ~offset:((i * 20) + 12) endian buf in
          entries := { tag; field; count; offset } :: !entries
    done;
    let entries = List.rev !entries in
    let data_offsets = get_dataset_offsets endian entries reader in
    let data_bytecounts = get_bytecounts endian entries reader in
    { entries; data_offsets; data_bytecounts; ro = reader; header }
end
  
type t = { header : header; ifd : Ifd.t }
let ifd t = t.ifd

let from_file (f : File.ro) =
  let header = header f in
  let ifd = Ifd.v ~file_offset:header.offset header f in
  { header; ifd }

type window = { xoff : int; yoff : int; xsize : int; ysize : int }

module Data = struct
  open Bigarray

  type ('repr, 'kind) kind =
    | Uint8 : (int, int8_unsigned_elt) kind
    | Float32 : (float, float32_elt) kind

  type ('repr, 'kind) t = ('repr, 'kind, c_layout) Genarray.t

  let ceil a b = (a + b - 1) / b

  let read_uint8_value buf buf_index i tiff_endianness =
    match tiff_endianness with _ -> Cstruct.get_uint8 buf (buf_index + i)

  let read_float32_value buf buf_index i tiff_endianness =
    (* Printf.printf "offset %i\n" (buf_index + (i * 4)); *)
    let int_value =
      Endian.uint32 ~offset:(buf_index + (i * 4)) tiff_endianness buf
    in
    Int32.float_of_bits int_value

  let read_data t ro window arr_type read_value =
    let ifd = t.ifd in
    let height = Ifd.height ifd in
    let strip_offsets = Ifd.data_offsets ifd in
    let strip_bytecounts = Ifd.data_bytecounts ifd in
    let rows_per_strip = Ifd.rows_per_strip ifd in
    let tiff_endianness = t.header.byte_order in
    let strip_offsets_length = List.length strip_offsets in
    if strip_offsets_length = List.length strip_bytecounts then (
      let arr_length = window.xsize * window.ysize in
      let arr = Genarray.create arr_type c_layout [| arr_length |] in
      let strip_offsets = Array.of_list strip_offsets in
      let strip_bytecounts = Array.of_list strip_bytecounts in
      let first_strip = window.yoff / rows_per_strip in
      (* assume whole numbers for now *)
      let last_strip = first_strip + ceil window.ysize rows_per_strip in
      let index = ref 0 in
      for s = first_strip to last_strip - 1 do
        let buf = Cstruct.create strip_bytecounts.(s) in
        let buf_index = ref 0 in
        let rows_in_strip =
          if s = strip_offsets_length - 1 then (* checking if its last strip*)
            height - (s * rows_per_strip)
          else
            rows_per_strip
        in
        let strip_length = strip_bytecounts.(s) / rows_in_strip in
        let opt_strip_offset = Optint.Int63.of_int strip_offsets.(s) in
        ro ~file_offset:opt_strip_offset [ buf ];
        for _ = 0 to rows_in_strip - 1 do
          for i = window.xoff to window.xoff + window.xsize - 1 do
            let value = read_value buf !buf_index i tiff_endianness in
            if !index < arr_length then Genarray.set arr [| !index |] value;
            index := !index + 1
          done;
          buf_index := !buf_index + strip_length
        done
      done;
      arr)
    else
      raise
        (Invalid_argument
           "strip_offsets and strip_bytecounts are of different lengths")

  let read_data_uint8 t ro window =
    read_data t ro window int8_unsigned read_uint8_value

  let read_data_float32 t ro window =
    read_data t ro window float32 read_float32_value
end

(* have to specify all 4 or else it defaults to whole file*)
let data (type repr kind) ?window t (f : File.ro)
    (data_type : (repr, kind) Data.kind) : (repr, kind) Data.t =
  let ifd = ifd t in
  let window =
    match window with
    | Some w -> w
    | None ->
        let width = Ifd.width ifd in
        let height = Ifd.height ifd in
        { xoff = 0; yoff = 0; xsize = width; ysize = height }
  in
  match data_type with
  | Data.Uint8 -> Data.read_data_uint8 t f window
  | Data.Float32 -> Data.read_data_float32 t f window
