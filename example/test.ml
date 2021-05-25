type t = { a : int }
[@@deriving serf { url = "example_url"; format = `Json example_fn }]

let () = print_endline "hello"
