open Ppxlib

let deriver = "serf"

(* a higher order fn for hygine. returns a fn that takes url option *)
let parse_options loc _url _format _meth =
  let format =
    match _format with
    | Some [%expr `Raw] -> `Raw
    | Some [%expr `Json [%e? fn]] ->
        `Json (Some fn) (* conversion fn/expression *)
    | Some [%expr `Json] | _ -> `Json None
    (* not quite sure if this is useful. maybe just make fn optional? *)
  in
  let url =
    match _url with
    | Some url -> url
    | None ->
        Location.raise_errorf ~loc
          "%s option \"url\" accepts a string constant parameter" deriver
  in
  let meth = match _meth with
    | Some [%expr `Delete] -> `Delete
    | Some [%expr `Get] | None -> `Get
    | Some [%expr `Patch] -> `Patch
    | Some [%expr `Post] -> `Post
    | Some [%expr `Put] -> `Put
    | Some [%expr [%e? x]] -> Location.raise_errorf ~loc:x.pexp_loc "invalid HTTP method"  in
  (url, format, meth)

let attr_key attrs =
  Ppx_deriving.(attrs |> attr ~deriver "key" |> Arg.(get_attr ~deriver expr))

let attr_default attrs =
  Ppx_deriving.(
    attrs |> attr ~deriver "default" |> Arg.(get_attr ~deriver expr))

let attr_ispostparam attrs =
  Ppx_deriving.(attrs |> attr ~deriver "post" |> Arg.get_flag ~deriver)

let attr_isgetparam attrs =
  Ppx_deriving.(attrs |> attr ~deriver "get" |> Arg.get_flag ~deriver)

let attr_ispathparam attrs =
  Ppx_deriving.(attrs |> attr ~deriver "path" |> Arg.get_flag ~deriver)

let is_optional { pld_type; pld_attributes; _ } =
  let attrs = pld_attributes @ pld_type.ptyp_attributes in
  match attr_default attrs with
  | Some _ -> true
  | None -> (
      match Ppx_deriving.remove_pervasives ~deriver pld_type with
      | [%type: [%t? _] list] | [%type: [%t? _] option] -> true
      | _ -> false)


let gen_name type_decl meth =
  let prefix =
  match meth with
    | `Get -> "serf_get"
    | `Delete -> "serf_delete"
    | `Patch -> "serf_patch"
    | `Post -> "serf_post"
    | `Put -> "serf_put"
  in
  match type_decl with
    | { ptype_name = { txt = "t"; _ }; _ } ->
        prefix
    | _ ->
        Ppx_deriving.mangle_type_decl (`Prefix prefix) type_decl


let generate_impl ~loc url format meth type_decl =
  let open Ast_builder.Default in
  let quoter = Ppx_deriving.create_quoter () in
  let creator =
    match type_decl.ptype_kind with
    | Ptype_record labels ->
      let fn =
        let formatter_exp =
          match format with
          | `Raw -> [%expr fun s -> s]
          | `Json None -> [%expr Yojson.Safe.from_string]
          | `Json (Some func) ->
              [%expr
                fun s ->
                  let open Result in
                  let mime =
                    Cohttp.(Header.get (Response.headers resp) "Content-Type")
                    |> function
                    | Some s -> s
                    | None -> ""
                  in
                  match String.trim (String.lowercase_ascii mime) with
                  | "application/json; charset=utf-8" | "application/json;"
                  | "application/json" | "" -> (
                      let json = Yojson.Safe.from_string s in
                      match [%e func] json with
                      | Ok _ as x -> x
                      | Error msg ->
                          Error
                            ("serf: the following fragment does not adhere \
                              to the expected schema (" ^ msg ^ "):\n"
                            ^ Yojson.Safe.pretty_to_string json
                            ^ "\n"))
                  | _ ->
                      Error
                        (Printf.sprintf
                            "bad response Content-Type (%s):expected (%s)" mime
                            "application/json; charset=utf-8")]
        in
        let req_exp =
          let meth_exp = match meth with
            | `Get -> [%expr `GET]
            | `Delete -> [%expr `DELETE]
            | `Patch -> [%expr `PATCH]
            | `Post -> [%expr `POST]
            | `Put -> [%expr `PUT]
          in [%expr Client.call ~headers ~body:(Cohttp_lwt.Body.of_string body) [%e meth_exp] uri]
        in
        let payload_exp =
          [%expr
          let headers =
            if List.length cookies > 0
              then
                let cookies = snd (Cohttp.Cookie.Cookie_hdr.serialize cookies) in
                Cohttp.Header.add headers "Cookie" cookies
              else headers
            in
            let%lwt resp, body =
              [%e req_exp]
            in
            let cookies = Cohttp.Cookie.Set_cookie_hdr.(extract resp.headers |> List.map (fun (_, x) -> x.cookie))
            in
            (* return a triple of the code, body, cookies *)
            match Code.code_of_status (Response.status resp) with
            | 200 ->
                let%lwt s = Cohttp_lwt.Body.to_string body in
                Lwt.return (200, [%e formatter_exp] s, cookies)
            | 301 ->
                Lwt.fail_with
                  (Printf.sprintf
                      "serf received HTTP response code 301, meaning that the \
                      requested resource has been moved.")
            | n ->
                let%lwt s = Cohttp_lwt.Body.to_string body in
                Lwt.return (n, [%e formatter_exp] s, cookies)]
        in
        pexp_fun ~loc Nolabel None (punit ~loc) payload_exp 
      in
      List.fold_right (fun { pld_name = { txt = name; loc }; pld_type; pld_attributes; pld_loc; _ } accum ->
        let attrs = pld_attributes @ pld_type.ptyp_attributes in
        let pld_type = Ppx_deriving.remove_pervasives ~deriver pld_type in
        let evar_name = evar ~loc name in
        let key = Option.default (estring ~loc name) (attr_key attrs)
        in
        (** The function that will be used at runtime to marshal this
          * parameter into a string or (nonempty) list of strings *)
        let rec make_converter pld_type =
          let t =
            (** We need to start by extracting the base type
              * @TODO figure out the desired semantics for [list option]s
              *   and [option list]s *)
            match pld_type with
              | [%type: [%t? t] list] -> t
              | [%type: [%t? t] option] -> t
              | [%type: [%t? t]] -> t
          in
          match t with
            | [%type: int] ->
                [%expr fun x -> [string_of_int x]]
            | [%type: bool] ->
                [%expr fun x -> [string_of_bool x]]
            | [%type: float] ->
                [%expr fun x -> [string_of_float x]]
            | [%type: string] ->
                [%expr ((fun x -> [x])[@inlined])]
            | {ptyp_desc = Ptyp_tuple fields; _} ->
                (** iterate over the tuple fields to get names and gen symbols *)
                let symbols = List.map (fun x -> x, gen_symbol()) fields in
                let fn_body =
                    let f (f, s ) = [%expr [%e make_converter f] [%e evar ~loc s]] in
                    (** create expression of converted tuple fields in list: string list list *)
                    List.fold_right (fun fs a -> [%expr [%e f fs] :: [%e a]]) symbols [%expr [[]]]
                in
                (** tuple arg pattern *)
                let tuple_bindings = List.map (pvar ~loc) (List.split symbols |> snd) in
                (** create the actual function. need to List.concat to get string list *)
                pexp_fun ~loc Nolabel None (ppat_tuple ~loc tuple_bindings) [%expr List.concat [%e fn_body]]
            | [%type: [%t? x]] ->
              let type_name = Ppxlib.string_of_core_type x in
                Location.raise_errorf ~loc:pld_loc "cannot derive %s for field '%s' of type '%s'" deriver name type_name
        in
        (** The converter needs to get wrapped with [List.map] if t is a
          * list type *)
        let converter =
          match pld_type with
            | [%type: [%t? _] list] ->
                [%expr
                  List.map ([%e make_converter pld_type])]
            | _ ->
                [%expr (fun x -> [%e make_converter pld_type] x)]
        in
        let add_to_uri_accum a =          
          [%expr
            let x = [%e converter] [%e evar_name] in
            let uri = Uri.add_query_param uri ([%e key], x) in
            [%e a]]
        in
        let add_path_to_uri_accum a =
          [%expr
            let [x] = [%e converter] [%e evar_name] in
            let path = Filename.concat (Uri.path uri) x in
            let uri = Uri.with_path uri path in
            [%e a]]
        in
        (* bootleg url-form-encoder *)
        (* TODO: this is really janked and kind of hardcoded in *)
        let add_post_param_accum a =
          [%expr
          let [x] = [%e converter] [%e evar_name] in
          let body =
            let form =
              let open Uri in
              let k = pct_encode ~component:`Query_key [%e key] in
              let v = pct_encode ~component:`Query_value x in
              k ^ "=" ^ v in
            if body = "" then  form
            else body ^ "&" ^ form
          in
          let headers = List.fold_left (fun a (k, v) -> Cohttp.Header.replace a k v) headers [
            "Content-Type", "application/x-www-form-urlencoded";
            "Content-Length", (string_of_int @@ String.length body)
          ] in
          [%e a]]
        in
        let add_body_accum a =
          [%expr
            let [x] = [%e converter] [%e evar_name] in
            let body = x in
            [%e a]]
        in
        let add_param_accum a =
          if attr_ispathparam attrs
            then add_path_to_uri_accum a
          else if name = "body"
            then add_body_accum a
          else if attr_ispostparam attrs
            then add_post_param_accum a
          else
            add_to_uri_accum a
        in
        match attr_default attrs with
          | Some default ->
            let default = Some (Ppx_deriving.quote ~quoter default) in
              pexp_fun ~loc (Optional name) default (pvar ~loc name) (add_param_accum accum)
          | None ->
            (* TODO: generalize this. we should be able to have
            optional post params, headers, etc.
            why are only optional query params allowed? *)
              begin match pld_type with
                | [%type: [%t? _] option] ->
                    let accum' =
                      [%expr
                        let uri = match [%e evar_name] with
                          | Some x ->
                            let x = [%e converter] x in
                            begin match x with
                              | [] -> raise (Failure ("parameter '" ^ [%e key] ^ "' is required"))
                              | x -> Uri.add_query_param uri ([%e key], x)
                            end
                          | None -> uri
                        in [%e accum]]
                    in
                    pexp_fun ~loc (Optional name) None (pvar ~loc name) accum'
                | _ ->
                    pexp_fun ~loc (Labelled name) None (pvar ~loc name) (add_param_accum accum)
              end)
        labels
        fn
    | _ ->
        Location.raise_errorf ~loc "%s can only be derived for record types"
          deriver
  in
  let uri = estring ~loc url in
  let creator = pexp_fun ~loc (Optional "headers") (Some [%expr []]) (pvar ~loc "headers") [%expr
    let headers = Cohttp.Header.of_list @@ ("User-Agent", "Mozilla/5.0") :: headers in
    [%e creator]
  ] |> pexp_fun ~loc (Optional "cookies") (Some [%expr []]) (pvar ~loc "cookies")
  in
  let creator =
    [%expr
      let open Cohttp in
      let open Cohttp_lwt_unix in
      let open Lwt in
      let open ExtLib in
      let uri = Uri.of_string [%e uri] in
      let body = "" in
      [%e creator]]
  in
  value_binding ~loc ~pat:(pvar ~loc (gen_name type_decl meth)) ~expr:(Ppx_deriving.sanitize ~quoter creator)

(* _rec_flag is recursiveness so we don't need it *)
let generate_impls ~ctxt (_rec_flag, type_decls) url format meth =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let url, format, meth = parse_options loc url format meth in
  List.map (generate_impl ~loc url format meth) type_decls
  |> Ast_builder.Default.(pstr_value_list ~loc Nonrecursive)

let generate_intf ~loc meth type_decl =
  let open Ast_builder.Default in
  let default_loc = Ast_helper.default_loc in 
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  let typ =
    match type_decl.ptype_kind with
      | Ptype_record labels ->
        let has_option = List.exists is_optional labels in
        let typ =
          match has_option with
            | true -> ptyp_arrow ~loc Nolabel (ptyp_constr ~loc {txt = Longident.parse "unit"; loc = !default_loc} []) typ
            | false -> typ
        in
        List.fold_left (fun accum { pld_name = { txt = name; loc }; pld_type; pld_attributes; _ } ->
          let attrs = pld_type.ptyp_attributes @ pld_attributes in
          let pld_type = Ppx_deriving.remove_pervasives ~deriver pld_type in
          match attr_default attrs with
            | Some _ -> ptyp_arrow ~loc (Optional name) pld_type accum
            | None ->
                begin match pld_type with
                  | [%type: [%t? _] list] ->
                      ptyp_arrow ~loc (Optional name) pld_type accum
                  | [%type: [%t? opt] option] ->
                      ptyp_arrow ~loc (Optional name) opt accum
                  | _ ->
                      ptyp_arrow ~loc (Labelled name) pld_type accum
                end)
          typ labels
      | _ -> Location.raise_errorf ~loc "%s can only be derived for record types" deriver
  in
  psig_value ~loc (value_description ~loc ~name:{txt = (gen_name type_decl meth); loc = !default_loc} ~type_:typ ~prim:[])

let generate_intfs ~ctxt (_rec_flag, type_decls) url format _meth =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let _, _, meth = parse_options loc url format _meth in
  List.map (generate_intf ~loc meth) type_decls

let impl_generator =
  Deriving.Generator.V2.make
    (* these arguments go after 'input_ast (kinda like a fn) *)
    Deriving.Args.(
      empty +> arg "url" (estring __) +> arg "format" __ +> arg "meth" __)
    generate_impls

let intf_generator =
  Deriving.Generator.V2.make
    Deriving.Args.(
      empty +> arg "url" (estring __) +> arg "format" __ +> arg "meth" __)
    generate_intfs

let my_deriver =
  Ppxlib.Deriving.add deriver ~str_type_decl:impl_generator
    ~sig_type_decl:intf_generator
