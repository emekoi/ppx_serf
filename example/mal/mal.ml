type anime_query = {
  query : string; [@key "q"]
  client_id : string; [@header "x-mal-client-id"]
}
[@@deriving
  serf
    {
      url = "https://api.myanimelist.net/v2/anime";
      meth = `Get;
      format = `Json;
    }]

let main () =
  let%lwt _, res, _ =
    serf_get_anime_query ~cookies:[] ~query:"Bepop"
      ~client_id:"6114d00ca681b7701d1e15fe11a4987e"
  in
  Lwt_io.print (Yojson.Safe.to_string res)

let () = Lwt_main.run (main ())
