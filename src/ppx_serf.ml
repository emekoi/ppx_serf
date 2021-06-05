open Ppxlib

let deriver = "serf"

(* a higher order fn for hygine. returns a fn that takes url option *)
let parse_options loc mangle _url _format =
  let format =
    match _format with
    | Some [%expr `Raw] -> `Raw
    | Some [%expr `Json [%e? fn]] ->
        `Json (Some fn) (* conversion fn/expression *)
    | Some [%expr `Json] | _ -> `Json None
    (* not quite sure if this is useful. maybe just make fn optional? *)
  in
  let url, meth =
    match _url with
    | Some [%expr `Delete [%e? x]] -> (x, `Delete)
    | Some [%expr `Get [%e? x]] -> (x, `Get)
    | Some [%expr `Patch [%e? x]] -> (x, `Patch)
    | Some [%expr `Post [%e? x]] -> (x, `Post)
    | Some [%expr `Put [%e? x]] -> (x, `Put)
    | Some [%expr [%e? x]] ->
        Location.raise_errorf ~loc:x.pexp_loc "invalid HTTP method"
    | None ->
        Location.raise_errorf ~loc
          "%s option \"url\" expected method with string parameter" deriver
  in
  let mangle =
    let f x = Ppx_deriving.string_of_expression_opt x in
    match mangle with
    | Some [%expr `Suffix [%e? x]] -> Some (`Suffix (f x))
    | Some [%expr `Suffix] -> Some (`Suffix None)
    | Some [%expr `Prefix [%e? x]] -> Some (`Prefix (f x))
    | Some [%expr `Prefix] -> Some (`Prefix None)
    | Some [%expr `PrefixSuffix [%e? x]] -> Some (`PrefixSuffix (f x))
    | Some [%expr `PrefixSuffix] -> Some (`PrefixSuffix None)
    | Some [%expr `SuffixPrefix [%e? x]] -> Some (`SuffixPrefix (f x))
    | Some [%expr `SuffixPrefix] -> Some (`SuffixPrefix None)
    | _ -> None
  in
  (mangle, url, format, meth)

let attr_key attrs =
  Ppx_deriving.(attrs |> attr ~deriver "key" |> Arg.(get_attr ~deriver expr))

let attr_default attrs =
  Ppx_deriving.(
    attrs |> attr ~deriver "default" |> Arg.(get_attr ~deriver expr))

let attr_isbodyparam attrs =
  Ppx_deriving.(attrs |> attr ~deriver "body" |> Arg.get_flag ~deriver)

let is_optional { pld_type; pld_attributes; _ } =
  let attrs = pld_attributes @ pld_type.ptyp_attributes in
  match attr_default attrs with
  | Some _ -> true
  | None -> (
      match Ppx_deriving.remove_pervasives ~deriver pld_type with
      | [%type: [%t? _] list] | [%type: [%t? _] option] -> true
      | _ -> false)

let gen_name type_decl mangle meth =
  let prefix =
    match meth with
    | `Get -> "get"
    | `Delete -> "delete"
    | `Patch -> "patch"
    | `Post -> "post"
    | `Put -> "put"
  in
  let prefix' =
    let app o f = Option.map_default f prefix o in
    Option.map
      (function
        | `Prefix x -> `Prefix (app x (fun p -> p ^ "_" ^ prefix))
        | `PrefixSuffix x -> `PrefixSuffix (app x Fun.id, prefix)
        | `Suffix x -> `Suffix (app x (fun s -> prefix ^ "_" ^ s))
        | `SuffixPrefix x -> `PrefixSuffix (prefix, app x Fun.id))
      mangle
  in
  let f x = Ppx_deriving.mangle_type_decl x type_decl in
  match type_decl with
  | { ptype_name = { txt = "t"; _ }; _ } -> Option.map_default f prefix prefix'
  | _ -> Option.default (`Prefix prefix) prefix' |> f

(* The function that will be used at runtime to marshal this
* parameter into a string or (nonempty) list of strings *)
let converter ~loc ~name ~quoter pld_type =
  let rec make_converter ~loc ~name ~quoter pld_type =
    let open Ast_builder.Default in
    let t =
      (* We need to start by extracting the base type
         * @TODO figure out the desired semantics for [list option]s
         *   and [option list]s *)
      match pld_type with
      | [%type: [%t? t] list] -> t
      | [%type: [%t? t] option] -> t
      | [%type: [%t? t]] -> t
    in
    match t with
    | [%type: int] -> [%expr fun x -> [ string_of_int x ]]
    | [%type: bool] -> [%expr fun x -> [ string_of_bool x ]]
    | [%type: float] -> [%expr fun x -> [ string_of_float x ]]
    | [%type: string] ->
        (*get a syntax error when adding a inline annotation*)
        [%expr fun x -> [ x ]]
    | { ptyp_desc = Ptyp_tuple fields; _ } ->
        (* iterate over the tuple fields to get names and gen symbols *)
        let symbols = List.map (fun x -> (x, gen_symbol ())) fields in
        let fn_body =
          let f (f, s) =
            [%expr [%e make_converter ~loc ~name ~quoter f] [%e evar ~loc s]]
          in
          (* create expression of converted tuple fields in list: string list list *)
          List.fold_right
            (fun fs a -> [%expr [%e f fs] :: [%e a]])
            symbols [%expr [ [] ]]
        in
        (* tuple arg pattern *)
        let tuple_bindings = List.map (pvar ~loc) (List.split symbols |> snd) in
        (* create the actual function. need to List.concat to get string list *)
        pexp_fun ~loc Nolabel None
          (ppat_tuple ~loc tuple_bindings)
          [%expr List.concat [%e fn_body]]
    | { ptyp_desc = Ptyp_constr _; _ } as x ->
        let full_path = Ppxlib.string_of_core_type x in
        let mname, tname =
          match Stringext.rcut ~on:"." full_path with
          | Some (m, t) -> (m ^ ".", t)
          | None -> ("", full_path)
        in
        let show_method =
          evar ~loc (mname ^ "show_" ^ tname) |> Ppx_deriving.quote ~quoter
        in
        [%expr fun x -> [ [%e show_method] x ]]
    | [%type: [%t? x]] ->
        let type_name = Ppxlib.string_of_core_type x in
        Location.raise_errorf ~loc
          "cannot derive %s for field '%s' of type '%s'" deriver name type_name
    (* The converter needs to get wrapped with [List.map] if t is a
       * list type *)
  in
  match pld_type with
  | [%type: [%t? _] list] ->
      [%expr
        fun x ->
          List.map [%e make_converter ~loc ~name ~quoter pld_type] x
          |> List.concat]
  | _ -> [%expr fun x -> [%e make_converter ~loc ~name ~quoter pld_type] x]

let generate_impl ~loc url format meth mangle type_decl =
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
            let meth_exp =
              match meth with
              | `Get -> [%expr `GET]
              | `Delete -> [%expr `DELETE]
              | `Patch -> [%expr `PATCH]
              | `Post -> [%expr `POST]
              | `Put -> [%expr `PUT]
            in
            [%expr
              uri_assoc :=
                List.filter_map
                  (function
                    | (key, [ with_ ]) as p ->
                        let pattern = "{" ^ key ^ "}" in
                        let uri' =
                          Stringext.replace_all !uri_ref ~pattern ~with_
                        in
                        if uri' = !uri_ref then Some p
                        else (
                          uri_ref := uri';
                          None)
                    | _ as p -> Some p)
                  !uri_assoc;
              let uri =
                Uri.of_string !uri_ref
                |> Fun.flip Uri.add_query_params (List.rev !uri_assoc)
              in
              Client.call ~headers
                ~body:(Cohttp_lwt.Body.of_string body)
                [%e meth_exp] uri]
          in
          let payload_exp =
            [%expr
              let headers =
                if List.length cookies > 0 then
                  let cookies =
                    snd (Cohttp.Cookie.Cookie_hdr.serialize cookies)
                  in
                  Cohttp.Header.add headers "Cookie" cookies
                else headers
              in
              let%lwt resp, body = [%e req_exp] in
              let cookies =
                Cohttp.Cookie.Set_cookie_hdr.(
                  extract resp.headers |> List.map (fun (_, x) -> x.cookie))
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
        List.fold_right
          (fun { pld_name = { txt = name; loc }; pld_type; pld_attributes; _ }
               accum ->
            let attrs = pld_attributes @ pld_type.ptyp_attributes in
            let pld_type = Ppx_deriving.remove_pervasives ~deriver pld_type in
            let evar_name = evar ~loc name in
            let key = Option.default (estring ~loc name) (attr_key attrs) in
            let add_to_uri_or_path_accum a =
              [%expr
                uri_assoc :=
                  ( [%e key],
                    [%e converter ~loc ~name ~quoter pld_type] [%e evar_name] )
                  :: !uri_assoc;
                [%e a]]
            in
            (* bootleg url-form-encoder *)
            (* TODO: this is really janked and kind of hardcoded in *)
            let add_post_param_accum a =
              [%expr
                let [ x ] =
                  [%e converter ~loc ~name ~quoter pld_type] [%e evar_name]
                in
                let body =
                  let form =
                    let open Uri in
                    let k = pct_encode ~component:`Query_key [%e key] in
                    let v = pct_encode ~component:`Query_value x in
                    k ^ "=" ^ v
                  in
                  if body = "" then form else body ^ "&" ^ form
                in
                let headers =
                  List.fold_left
                    (fun a (k, v) -> Cohttp.Header.replace a k v)
                    headers
                    [
                      ("Content-Type", "application/x-www-form-urlencoded");
                      ("Content-Length", string_of_int @@ String.length body);
                    ]
                in
                [%e a]]
            in
            let add_body_accum a =
              [%expr
                let [ x ] =
                  [%e converter ~loc ~name ~quoter pld_type] [%e evar_name]
                in
                let body = x in
                [%e a]]
            in
            let add_param_accum a =
              if attr_isbodyparam attrs && meth == `Post then
                add_post_param_accum a
              else if name = "body" then add_body_accum a
              else add_to_uri_or_path_accum a
            in
            match attr_default attrs with
            | Some default ->
                let default = Some (Ppx_deriving.quote ~quoter default) in
                pexp_fun ~loc (Optional name) default (pvar ~loc name)
                  (add_param_accum accum)
            | None -> (
                (* TODO: generalize this. we should be able to have
                   optional post params, headers, etc.
                   why are only optional query params allowed? *)
                match pld_type with
                | [%type: [%t? _] option] ->
                    let accum' =
                      [%expr
                        (match [%e evar_name] with
                        | Some x -> (
                            let x =
                              [%e converter ~loc ~name ~quoter pld_type] x
                            in
                            match x with
                            | [] ->
                                raise
                                  (Failure
                                     ("parameter '" ^ [%e key] ^ "' is required"))
                            | x -> uri_assoc := ([%e key], x) :: !uri_assoc)
                        | None -> ());
                        [%e accum]]
                    in
                    pexp_fun ~loc (Optional name) None (pvar ~loc name) accum'
                | _ ->
                    pexp_fun ~loc (Labelled name) None (pvar ~loc name)
                      (add_param_accum accum)))
          labels fn
    | _ ->
        Location.raise_errorf ~loc "%s can only be derived for record types"
          deriver
  in
  let creator =
    pexp_fun ~loc (Optional "headers")
      (Some [%expr []])
      (pvar ~loc "headers")
      [%expr
        let headers =
          Cohttp.Header.of_list @@ ("User-Agent", "Mozilla/5.0") :: headers
        in
        [%e creator]]
    |> pexp_fun ~loc (Optional "cookies")
         (Some [%expr []])
         (pvar ~loc "cookies")
  in
  let creator =
    [%expr
      let open Cohttp in
      let open Cohttp_lwt_unix in
      let open Lwt in
      let open ExtLib in
      let uri_assoc = ref [] in
      let uri_ref = ref [%e url] in
      let body = "" in
      [%e creator]]
  in
  value_binding ~loc
    ~pat:(pvar ~loc (gen_name type_decl mangle meth))
    ~expr:(Ppx_deriving.sanitize ~quoter creator)

(* _rec_flag is recursiveness so we don't need it *)
let generate_impls ~ctxt (_rec_flag, type_decls) mangle url format =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let mangle, url, format, meth = parse_options loc mangle url format in
  List.map (generate_impl ~loc url format meth mangle) type_decls
  |> Ast_builder.Default.(pstr_value_list ~loc Nonrecursive)

let generate_intf ~loc meth mangle type_decl =
  let open Ast_builder.Default in
  let default_loc = Ast_helper.default_loc in
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  let typ =
    match type_decl.ptype_kind with
    | Ptype_record labels ->
        let has_option = List.exists is_optional labels in
        let typ =
          match has_option with
          | true ->
              ptyp_arrow ~loc Nolabel
                (ptyp_constr ~loc
                   { txt = Longident.parse "unit"; loc = !default_loc }
                   [])
                typ
          | false -> typ
        in
        List.fold_left
          (fun accum
               { pld_name = { txt = name; loc }; pld_type; pld_attributes; _ } ->
            let attrs = pld_type.ptyp_attributes @ pld_attributes in
            let pld_type = Ppx_deriving.remove_pervasives ~deriver pld_type in
            match attr_default attrs with
            | Some _ -> ptyp_arrow ~loc (Optional name) pld_type accum
            | None -> (
                match pld_type with
                | [%type: [%t? _] list] ->
                    ptyp_arrow ~loc (Optional name) pld_type accum
                | [%type: [%t? opt] option] ->
                    ptyp_arrow ~loc (Optional name) opt accum
                | _ -> ptyp_arrow ~loc (Labelled name) pld_type accum))
          typ labels
    | _ ->
        Location.raise_errorf ~loc "%s can only be derived for record types"
          deriver
  in
  psig_value ~loc
    (value_description ~loc
       ~name:{ txt = gen_name type_decl mangle meth; loc = !default_loc }
       ~type_:typ ~prim:[])

let generate_intfs ~ctxt (_rec_flag, type_decls) mangle url format =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let mangle, _, _, meth = parse_options loc mangle url format in
  List.map (generate_intf ~loc meth mangle) type_decls

let impl_generator =
  Deriving.Generator.V2.make
    (* these arguments go after 'input_ast (kinda like a fn) *)
    Deriving.Args.(empty +> arg "mangle" __ +> arg "url" __ +> arg "format" __)
    generate_impls

let intf_generator =
  Deriving.Generator.V2.make
    Deriving.Args.(empty +> arg "mangle" __ +> arg "url" __ +> arg "format" __)
    generate_intfs

let my_deriver =
  Ppxlib.Deriving.add deriver ~str_type_decl:impl_generator
    ~sig_type_decl:intf_generator
