open Lwt
open Http_state


let find key (yaml : Yaml.value) = match yaml with 
  | `O assoc -> List.assoc_opt key assoc
  | _ -> None

let string_of_yaml value =
  let data = Yaml.to_string value in
  match data with
  | Ok x -> String.trim x
  | _ -> failwith "yaml parsing error"

let int_of_yaml value =
  let data = Yaml.to_string value in
  match data with
  | Ok x -> x
  | _ -> failwith "yaml parsing error"

let find_with_exc key (yaml : Yaml.value) func =
  match find key yaml with
  | Some value -> func value
  | None -> failwith (Printf.sprintf "key: %s not found" key)


module Server = struct
  let create_state yaml =
    {
      host=find_with_exc "host" yaml string_of_yaml;
      port=find_with_exc "port" yaml string_of_yaml;
      cgi_path=find_with_exc "cgi_path" yaml string_of_yaml;
    }
  let init config_file_path =
    let raw = Yaml_unix.of_file Fpath.(v config_file_path) in
    match raw with
      | Ok(data) -> data |> create_state
      | _ -> failwith "config file not found"
  
  let run state =
    Lwt_main.run ((Socket.create_socket state) >>= fun sock ->
      let serve = Socket.create_server sock state in serve ())
      
end
