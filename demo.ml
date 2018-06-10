open! Core_kernel
open Incr_dom
open Async_kernel

module Model = struct
  type t = {history: Navigation.location list}
  [@@deriving sexp, fields, compare]

  let update_history t location = {history= location :: t.history}

  let cutoff t1 t2 = compare t1 t2 = 0
end

module Action = struct
  type t = UrlChange of Navigation.location
  [@@deriving sexp]

  let should_log _ = true
end

module State = struct
  type t = {schedule: Action.t -> unit} [@@deriving fields]
end

let apply_action action model state =
  match (action : Action.t) with
  | UrlChange location -> Model.update_history model location

let update_visibility m = m

(** Listen for the Dom hash change event.
    This returns a Deferred that will be fulfilled
    when the browser hashchange event is fired *)
let route_change_event () =
  let open Js_of_ocaml in
  let el = ref Js.null in
  let ivar = Ivar.create () in
  el := Js.some
      (Dom.addEventListener
         Dom_html.window
         Dom_html.Event.hashchange (Dom_html.handler
                                      (fun (ev : #Dom_html.event Js.t) ->
                                         (Ivar.fill ivar (
                                             Navigation.location_of_js (Dom_html.window##.location))); Js._true)) (Js.bool true));
  Ivar.read ivar

(** Bind to the hash change event for the lifecycle of the application. *)
let rec watch_route_changes ~schedule =
  let%bind ev = route_change_event () in
  schedule (Action.UrlChange ev);
  watch_route_changes ~schedule

let on_startup ~schedule _ =
  let state = {State.schedule} in
  let _ = watch_route_changes ~schedule in
  Async_kernel.return state

let on_display ~old:_ _ _ = ()

let view (m: Model.t Incr.t) ~inject =
  let open Incr.Let_syntax in
  let open Vdom in
  let view_link name =
    Node.li [] [Node.a [Attr.href ("#" ^ "/" ^ name)] [Node.text name]]
  in
  let view_location location =
    Node.li [] [Node.text ((Navigation.pathname location) ^ (Navigation.hash location))]
  in
  let%map history =
    let%map history_list = m >>| Model.history in
    Node.ul [] (List.map ~f:(fun x -> view_location x) history_list)
  in
  Node.body []
    [ Node.div []
        [ Node.h1 [] [Node.text "Pages"]
        ; Node.ul []
            (List.map ~f:view_link
               ["bears"; "cats"; "dogs"; "elephants"; "fish"]) ]
    ; Node.h1 [] [Node.text "History"]
    ; history ]
