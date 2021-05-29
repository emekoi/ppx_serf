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
                    | s ->
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
              let headers = Cohttp.Header.of_list ["User-Agent", "Mozilla/5.0"; Cohttp.Cookie.Cookie_hdr.serialize cookies]
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
          pexp_fun ~loc (Optional "cookies")
            (Some [%expr []])
            (pvar ~loc "cookies")
          payload_exp
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
                [%expr (string_of_int)]
            | [%type: bool] ->
                [%expr (string_of_bool)]
            | [%type: float] ->
                [%expr (string_of_float)]
            | [%type: string] ->
                [%expr ((fun x -> x)[@inlined])]
            | [%type: [%t? t1] * [%t? t2]] -> (* I'm so sorry *)
                let c1, c2 = make_converter t1, make_converter t2 in
                [%expr (fun (a, b) -> ([%e c1] a) ^ "," ^ ([%e c2] b))]
            | [%type: [%t? t1] * [%t? t2] * [%t? t3]] ->
                (* I'll never use anything bigger than a 3-tuple, right? *)
                let c1, c2, c3 = make_converter t1, make_converter t2, make_converter t3 in
                [%expr (fun (a, b, c) -> ([%e c1] a) ^ "," ^ ([%e c2] b) ^ "," ([%e c3]))]
            | [%type: [%t? _]] ->
                Location.raise_errorf ~loc:pld_loc "cannot derive %s for field '%s'" deriver name
        in
        (** The converter needs to get wrapped with [List.map] if t is a
          * list type *)
        let converter =
          match pld_type with
            | [%type: [%t? _] list] ->
                [%expr
                  List.map ([%e make_converter pld_type])]
            | _ ->
                [%expr (fun x -> [[%e make_converter pld_type] x])]
        in
        let add_to_uri_accum =          
          [%expr
            let x = [%e converter] [%e evar_name] in
            let uri = Uri.add_query_param uri ([%e key], x) in
            [%e accum]]
        in
        let add_path_to_uri_accum =
          [%expr
            let [x] = [%e converter] [%e evar_name] in
            let path = Filename.concat (Uri.path uri) x in
            let uri = Uri.with_path uri path in
            [%e accum]]
        in
        let add_post_param_accum =
          [%expr
            let [x] = [%e converter] [%e evar_name] in
            let body =
              begin match body with
                | "" ->
                    (Uri.pct_encode [%e key]) ^ "=" ^ (Uri.pct_encode x)
                | s ->
                    s ^ "&" ^ (Uri.pct_encode [%e key]) ^ "=" ^ (Uri.pct_encode x)
              end
            in
            [%e accum]]
        in
        let add_body_accum =
          [%expr
            let [x] = [%e converter] [%e evar_name] in
            let body = x in
            [%e accum]]
        in
        let addparam_accum =
          match attr_ispathparam attrs with
            | true ->
                add_path_to_uri_accum
            | false ->
                if name = "body"
                then add_body_accum
                else begin match attr_ispostparam attrs with
                  | true ->
                      add_post_param_accum
                  | false ->
                      add_to_uri_accum
                end
        in
        match attr_default attrs with
          | Some default ->
            let default = Some (Ppx_deriving.quote ~quoter default) in
              pexp_fun ~loc (Optional name) default (pvar ~loc name) addparam_accum
          | None ->
              begin match pld_type with
                | [%type: [%t? _] option] ->
                    let accum' =
                      [%expr
                        let uri =
                          match [%e evar_name] with
                            | Some x ->
                                let x = [%e converter] x in
                                begin match x with
                                  | [] -> raise (Failure ("parameter is required"))
                                  | x -> Uri.add_query_param uri ([%e key], x)
                                end
                            | None -> uri
                        in [%e accum]]
                    in
                    pexp_fun ~loc (Optional name) None (pvar ~loc name) accum'
                | _ ->
                    pexp_fun ~loc (Labelled name) None (pvar ~loc name) addparam_accum
              end)
        labels
        fn
    | _ ->
        Location.raise_errorf ~loc "%s can only be derived for record types"
          deriver
  in
  let uri = estring ~loc url in
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
let generate_impls ~ctxt (_rec_flag, type_decls) _url _format _meth =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let url, format, meth = parse_options loc _url _format _meth in
  List.map (generate_impl ~loc url format meth) type_decls
  |> Ast_builder.Default.(pstr_value_list ~loc Nonrecursive)

let generate_intf ~loc _url _format meth type_decl =
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

let generate_intfs ~ctxt (_rec_flag, type_decls) _url _format _meth =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let url, format, meth = parse_options loc _url _format _meth in
  List.map (generate_intf ~loc url format meth) type_decls

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
