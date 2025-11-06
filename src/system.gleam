import envoy
import gleam/result
import gleam/string

pub fn get_path_dirs() -> List(String) {
  let path = result.unwrap(envoy.get("PATH"), "")

  case is_windows() {
    True -> string.split(path, ";")
    False -> string.split(path, ":")
  }
}

@external(erlang, "erlang", "halt")
pub fn exit(status: Int) -> Nil

@external(erlang, "filepath_ffi", "is_windows")
fn is_windows() -> Bool

@external(erlang, "filename", "join")
pub fn path_join(from: String, to: String) -> String
