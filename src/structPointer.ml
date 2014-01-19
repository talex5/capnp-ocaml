module Int64 = Core.Core_int64

type t = {
  offset        : int;
  data_size     : int;
  pointers_size : int;
}

let offset_shift = 2
let offset_mask  = Int64.shift_left (Int64.of_int 0x3fffffff) offset_shift

let data_size_shift = 32
let data_size_mask  = Int64.shift_left (Int64.of_int 0xffff) data_size_shift

let pointers_size_shift = 48
let pointers_size_mask  = Int64.shift_left (Int64.of_int 0xffff) pointers_size_shift

let decode (pointer64 : Int64.t) : t =
  let offset =
    let masked     = Int64.bit_and pointer64 offset_mask in
    let offset64   = Int64.shift_right_logical masked offset_shift in
    let offset_int = Int64.to_int_exn offset64 in
    Util.decode_signed 30 offset_int
  in
  let data_size =
    let masked = Int64.bit_and pointer64 data_size_mask in
    let size64 = Int64.shift_right_logical masked data_size_shift in
    Int64.to_int_exn size64
  in
  let pointers_size =
    let masked = Int64.bit_and pointer64 pointers_size_mask in
    let size64 = Int64.shift_right_logical masked pointers_size_shift in
    Int64.to_int_exn size64
  in {
    offset;
    data_size;
    pointers_size;
  }