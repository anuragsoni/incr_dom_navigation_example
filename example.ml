open! Core_kernel
open! Incr_dom
open! Js_of_ocaml
let () =
  Start_app.simple
  (module Demo)
  ~initial_model:(Demo.Model.Fields.create ~message:"" ~history:[])
