# ppx_serf
build API clients from type definitions.
## example
```ocaml
open Ppx_deriving_yojson_runtime

type anime_quote = { anime : string; character : string; quote : string }

and anime_quotes = anime_quote list [@@deriving yojson]

type anime_spec = { title : string }
[@@deriving
  serf
    {
      url = "https://animechan.vercel.app/api/quotes/anime";
      meth = `Get;
      format = `Json anime_quotes_of_yojson;
    }]

let main () =
  let title = Sys.argv.(1) in
  let%lwt _, res, _ = serf_get_anime_spec ~title ~cookies:[] in
  match res with
  | Ok res ->
      Lwt_list.iter_s
        (fun { quote; character; _ } ->
          Lwt_io.printf "%s - \"%s\"\n\n" character quote)
        res
  | Error msg -> Lwt_io.printf "Problem decoding response: %s\n" msg

let () = Lwt_main.run (main ())
```
## notes
to derive `serf` for records containing enums, `show` must derived for those fields as well.