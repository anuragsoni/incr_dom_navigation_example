open! Core_kernel
open Incr_dom
open Async_kernel

module Model = struct
  type t = {message: string; history: string list}
  [@@deriving sexp, fields, compare]

  let update t message = {t with message}

  let update_history t location = {t with history= location :: t.history}

  let cutoff t1 t2 = compare t1 t2 = 0
end

module Action = struct
  type t = Update of string | UrlChange of Navigation.location
  [@@deriving sexp]

  let should_log _ = true
end

module State = struct
  type t = {schedule: Action.t -> unit} [@@deriving fields]
end

let apply_action action model state =
  match (action : Action.t) with
  | Update msg -> Model.update model msg
  | UrlChange location -> Model.update_history model location.hash

let update_visibility m = m

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

let on_startup ~schedule _ =
  let state = {State.schedule} in
  upon (route_change_event ()) (fun ev -> State.schedule state (Action.UrlChange ev));
  Async_kernel.return state

let on_display ~old:_ _ _ = ()

let view (m: Model.t Incr.t) ~inject =
  let open Incr.Let_syntax in
  let open Vdom in
  let view_link name =
    Node.li [] [Node.a [Attr.href ("#" ^ "/" ^ name)] [Node.text name]]
  in
  let%map message =
    let%map message_text = m >>| Model.history in
    Node.ul [] (List.map ~f:(fun x -> Node.text x) message_text)
  in
  Node.body []
    [ Node.div []
        [ Node.h1 [] [Node.text "Pages"]
        ; Node.ul []
            (List.map ~f:view_link
               ["bears"; "cats"; "dogs"; "elephants"; "fish"]) ]
    ; Node.h1 [] [Node.text "History"]
    ; message ]
