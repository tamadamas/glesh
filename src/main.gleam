import gleam/io
import input.{input}

pub fn main() {
  let assert Ok(user_input) = input(prompt: "$ ")
  print_not_found_message(user_input)
}

fn print_not_found_message(command: String) {
  io.println(command <> ": command not found")
}
