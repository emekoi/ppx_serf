open Ppxlib

let deriver = "serf"

(* let unwrap = function Some a -> a | None -> raise (Failure "invalid unwrap") *)

(* a higher order fn for hygine. returns a fn that takes url option *)
let parse_options loc _meth =
  let meth =
    match _meth with
    | Some [%expr `Json] -> `Json None
    | Some [%expr `Json [%e? fn]] ->
        `Json (Some fn) (* conversion fn/expression *)
    | _ -> `None
    (* not quite sure if this is useful. maybe just make fn optional? *)
  in
  function
  | Some url -> (meth, url)
  | None ->
      Location.raise_errorf ~loc
        "%s option \"url\" accepts a string constant parameter" deriver

let generate_impl ~ctxt (_rec_flag, _type_declarations) _url _meth =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let _, _ = parse_options loc _meth _url in
  []

let generate_intf ~ctxt (_rec_flag, _type_declarations) _url _meth =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let _, _ = parse_options loc _meth _url in
  []

let impl_generator =
  Deriving.Generator.V2.make
    (* these arguments go after 'input_ast (kinda like a fn) *)
    Deriving.Args.(empty +> arg "url" (estring __) +> arg "format" __)
    generate_impl

let intf_generator =
  Deriving.Generator.V2.make
    Deriving.Args.(empty +> arg "url" (estring __) +> arg "format" __)
    generate_intf

let my_deriver =
  Deriving.add deriver ~str_type_decl:impl_generator
    ~sig_type_decl:intf_generator
