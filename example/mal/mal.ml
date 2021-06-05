type anime_query = { query : string [@key "q"] [@default "joe"] }
[@@deriving
  serf { url = `Get "https://api.myanimelist.net/v2/anime"; format = `Json }]

let main () =
  let query = Sys.argv.(1) in
  let%lwt _, res, _ =
    get_anime_query
      ~headers:[ ("x-mal-client-id", "6114d00ca681b7701d1e15fe11a4987e") ]
      ~query ()
  in
  Lwt_io.print (Yojson.Safe.to_string res)

let () = Lwt_main.run (main ())
