type anime_query = {
  query : string; [@key "q"] [@default "joe"]
  client_id : string;
      [@header "x-mal-client-id"] [@default "6114d00ca681b7701d1e15fe11a4987e"]
}
[@@deriving
  serf
    {
      url = "https://api.myanimelist.net/v2/anime";
      meth = `Get;
      format = `Json;
    }]

let main () =
  let query = Sys.argv.(1) in
  let%lwt _, res, _ = serf_get_anime_query ~query () in
  Lwt_io.print (Yojson.Safe.to_string res)

let () = Lwt_main.run (main ())
