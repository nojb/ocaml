open Syntax

(* utf8 flag *)
let utf8_mode = ref false

let rg a b = Characters (Cset.interval a b)

let interval a b =
  let a = min a b and b = max a b in
  let lo x i = 0x80 lor ((x lsr (6 * i)) land 0x3f) in
  let r = ref [] in
  if a <= 0x7f then begin
    let b = min b 0x7f in
    r := [rg a b]
  end;
  if a <= 0x7ff then begin
    let b = min b 0x7ff in
    let hi x = 0xc0 lor ((x lsr 6) land 0x1f) in
    let a0 = lo a 0 and a1 = hi a in
    let b0 = lo b 0 and b1 = hi b in
    r := seq [rg a1 b1; rg a0 b0] :: !r
  end;
  if a <= 0xffff then begin
    let b = min b 0xffff in
    let hi x = 0xe0 lor ((x lsr 12) land 0xf) in
    let a0 = lo a 0 and a1 = lo a 1 and a2 = hi a in
    let b0 = lo b 0 and b1 = lo b 1 and b2 = hi b in
    r := seq [rg a2 b2; rg a1 b1; rg a0 b0] :: !r
  end;
  if a <= 0x1fffff then begin
    let b = min b 0x1fffff in
    let hi x = 0xf0 lor ((x lsr 16) land 0x7) in
    let a0 = lo a 0 and a1 = lo a 1 and a2 = lo a 2 and a3 = hi a in
    let b0 = lo b 0 and b1 = lo b 1 and b2 = lo b 2 and b3 = hi b in
    r := seq [rg a3 b3; rg a2 b2; rg a1 b1; rg a0 b0] :: !r
  end;
  !r
let all_chars =
  [0, 0x10ffff]
