(* open Lwt *)

type address = {
  address_components : Yojson.Safe.t;
  formatted_address : string;
  geometry : Yojson.Safe.t;
  place_id : Yojson.Safe.t;
  types : Yojson.Safe.t;
}
[@@deriving yojson]

type http_result = { results : address list; status : string }
[@@deriving yojson]

type address_of_coords = { latlng : float; key : string }
[@@deriving
  serf
    {
      url = "https://maps.googleapis.com/maps/api/geocode/json";
      meth = `Get;
      format = `Json http_result_of_yojson;
    }]

let main () =
  let lat, lng = (float_of_string Sys.argv.(1), float_of_string Sys.argv.(2)) in
  let%lwt _, res, _ =
    serf_get_address_of_coords ~latlng:lat ~key:"0123456789abcdef" ~cookies:[]
  in
  match res with
  | Ok { results = addrs; status = "OK" } ->
      Lwt_list.iter_s
        (fun addr ->
          Lwt_io.printf "The coordinates %f,%f correspond to the address %s\n"
            lat lng addr.formatted_address)
        addrs
  | Ok res -> Lwt_io.printf "HTTP fuckup: %s\n" res.status
  | Error msg -> Lwt_io.printf "Problem decoding response: %s\n" msg

let () = Lwt_main.run (main ())
