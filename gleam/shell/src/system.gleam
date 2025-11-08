import envoy
import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/list
import gleam/result
import gleam/string

pub type ExecOutput {
  StdOutOutput(String)
  StdErrOutput(String)
}

pub fn get_path_dirs() -> List(String) {
  let path = result.unwrap(envoy.get("PATH"), "")

  case is_windows() {
    True -> string.split(path, ";")
    False -> string.split(path, ":")
  }
}

@external(erlang, "erlang", "halt")
pub fn exit(status: Int) -> Nil

@external(erlang, "erlang_ffi", "is_windows")
fn is_windows() -> Bool

@external(erlang, "filename", "join")
pub fn path_join(from: String, to: String) -> String

pub fn run_cmd(
  name: String,
  args: List(String),
) -> Result(List(ExecOutput), String) {
  let command = name <> " " <> string.join(args, " ")

  case run_shell_cmd(command, shell_default_options()) {
    Ok(output) -> decode_exec_output(output)
    Error(error) -> decode_exec_output(error)
  }
}

@external(erlang, "gleam_erlang_ffi", "identity")
pub fn from_dynamic_to_tuple(a: dynamic.Dynamic) -> #(String, List(String))

fn decode_exec_output(output: Dynamic) -> Result(List(ExecOutput), String) {
  let atom_decoder = atom.decoder() |> decode.map(atom.to_string)
  let decoder =
    decode.field(0, atom_decoder, fn(key) {
      use message <- decode.field(
        1,
        decode.list(decode.bit_array) |> decode.map(bit_array.concat),
      )
      decode.success(build_exec_ouput(key, message))
    })

  case decode.run(output, decode.list(decoder)) {
    Ok(entries) -> Ok(entries)
    Error(_errors) -> Error("Something went wrong")
  }
}

// fn collect_decoder_errors(errors: List(decode.DecodeError)) -> String {
//   list.fold(errors, from: "", with: fn(acc, error) {
//     acc
//     <> "Expected: "
//     <> error.expected
//     <> "\nFound: "
//     <> error.found
//     <> "\nPath:"
//     <> string.join(error.path, " => ")
//   })
// }

fn build_exec_ouput(key: String, message: BitArray) -> ExecOutput {
  let message = bit_array.to_string(message) |> result.unwrap("")

  case key {
    "stdout" -> StdOutOutput(message)
    "stderr" -> StdErrOutput(message)
    _ -> StdErrOutput(message)
  }
}

fn shell_default_options() -> List(atom.Atom) {
  ["sync", "stdout", "stderr"]
  |> list.try_map(atom.get)
  |> result.unwrap([])
}

@external(erlang, "exec", "run")
fn run_shell_cmd(
  command: String,
  options: List(atom.Atom),
) -> Result(Dynamic, Dynamic)
