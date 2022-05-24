(* TEST
   * expect
*)

module X = struct
  type t = {x: int}
  let print [@toplevel_printer] = fun ppf {x} ->
    Format.fprintf ppf "X.x = %d" x
end;;
{X.x = 12};;
[%%expect {|
module X :
  sig type t = { x : int; } val print : Format.formatter -> t -> unit end
- : X.t = X.x = 12
|}]

module Y : sig
  type t
  val sample : t
  val print : Format.formatter -> t -> unit
  [@@toplevel_printer]
end = struct
  type t = {x: int}
  let sample = {x = 42}
  let print [@toplevel_printer] = fun ppf {x} ->
    Format.fprintf ppf "Y.x = %d" x
end;;
Y.sample;;
[%%expect {|
module Y :
  sig type t val sample : t val print : Format.formatter -> t -> unit end
- : Y.t = Y.x = 42
|}]

module type U = sig
  type t
  val sample : t
  val print : Format.formatter -> t -> unit
  [@@toplevel_printer]
end

module Z : U = struct
  type t = {x: int}
  let sample = {x = 42}
  let print ppf {x} = Format.fprintf ppf "Z.x = %d" x
end;;
Z.sample;;
[%%expect {|
module type U =
  sig type t val sample : t val print : Format.formatter -> t -> unit end
module Z : U
- : Z.t = Z.x = 42
|}]

module F (A : sig type t val to_string : t -> string end) : sig
  val print : Format.formatter -> A.t -> unit
  [@@toplevel_printer]
end = struct
  let print ppf x = Format.fprintf ppf "%s" (A.to_string x)
end

module A = struct type t = {x : int} let to_string {x} = Printf.sprintf "M.x = %d" x end
module M = F (A);;
{A.x = 101};;
[%%expect {|
module F :
  functor (A : sig type t val to_string : t -> string end) ->
    sig val print : Format.formatter -> A.t -> unit end
module A : sig type t = { x : int; } val to_string : t -> string end
module M : sig val print : Format.formatter -> A.t -> unit end
- : A.t = M.x = 101
|}]
