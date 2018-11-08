let () = Sheep.baa Sheep.s (* Use Sheep module *)
let _ = fun (x : Pig.t) -> x (* Reference Pig module *)

(* Test that a privately loaded module cannot have the same name as a
   module in the program. *)
let test_sheep () =
  match
    if Dynlink.is_native then
      Dynlink.loadfile_private "plugin1/sheep.cmxs"
    else
      Dynlink.loadfile_private "plugin1/sheep.cmo"
  with
  | () -> assert false
  | exception Dynlink.Error (
      Dynlink.Module_already_loaded "Sheep") -> ()

(* Test repeated loading of a privately-loaded module. *)
let test_cow_repeated () =
  if Dynlink.is_native then
    Dynlink.loadfile_private "plugin2/cow.cmxs"
  else
    Dynlink.loadfile_private "plugin2/cow.cmo"

(* Test that a privately loaded module can have the same name as a
   previous privately loaded module, in the case where the interfaces are
   the same, but the implementations differ. *)
let test_cow_same_name_same_mli () =
  if Dynlink.is_native then
    Dynlink.loadfile_private "plugin2b/cow.cmxs"
  else
    Dynlink.loadfile_private "plugin2b/cow.cmo"

(* Test that a privately loaded module can have the same name as a
   previous privately loaded module, in the case where neither the interfaces
   nor the implementations are the same. *)
let test_cow_same_name_different_mli () =
  if Dynlink.is_native then
    Dynlink.loadfile_private "plugin2c/cow.cmxs"
  else
    Dynlink.loadfile_private "plugin2c/cow.cmo"

(* Test that a privately loaded module cannot have the same name as an
   interface depended on by modules the program. *)
let test_pig () =
  match
    if Dynlink.is_native then
      Dynlink.loadfile_private "plugin3/pig.cmxs"
    else
      Dynlink.loadfile_private "plugin3/pig.cmo"
  with
  | () -> assert false
  | exception Dynlink.Error (
      Dynlink.Private_library_cannot_implement_interface "Pig") -> ()

(* Test that a privately loaded module can recursively load a module of
   the same name. *)
let test_chicken () =
  if Dynlink.is_native then
    Dynlink.loadfile_private "plugin4/chicken.cmxs"
  else
    Dynlink.loadfile_private "plugin4/chicken.cmo"

(* Test that a public load of a module M inside a privately-loaded module,
   followed by a public load of M, causes an error. *)
let test_pheasant () =
  begin
    if Dynlink.is_native then
      Dynlink.loadfile_private "plugin6/pheasant.cmxs"
    else
      Dynlink.loadfile_private "plugin6/pheasant.cmo"
  end;
  match
    if Dynlink.is_native then
      Dynlink.loadfile "plugin6/partridge.cmxs"
    else
      Dynlink.loadfile "plugin6/partridge.cmo"
  with
  | () -> assert false
  | exception Dynlink.Error (
      Dynlink.Module_already_loaded "Partridge") -> ()

let () =
  test_sheep ();
  test_cow_repeated ();
  test_cow_repeated ();
  test_cow_same_name_same_mli ();
  test_cow_same_name_different_mli ();
  test_pig ();
  test_chicken ();
  test_pheasant ()
