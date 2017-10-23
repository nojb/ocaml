let () =
  let oc = open_out_bin "foo.txt" in
  close_out oc;
  let t0 = 1508391026. in
  Unix.utimes "foo.txt" t0 t0;
  let t1 = (Unix.stat "foo.txt").Unix.st_mtime in
  Printf.printf "%.0f = %.0f (%B)\n%!" t0 t1 (t0 = t1)
