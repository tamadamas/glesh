import gleam/io
import input.{input}

const not_found_command = ": command not found"

pub fn main() {
  run_repl()
}

fn run_repl() {
  let assert Ok(user_input) = input(prompt: "$ ")
  print_not_found_message(user_input)

  run_repl()
}

fn print_not_found_message(command: String) {
  io.println(command <> not_found_command)
}
