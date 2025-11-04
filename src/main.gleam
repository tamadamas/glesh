import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import gleave.{exit}
import input.{input}

const not_found_command = ": command not found"

pub fn main() {
  run_repl()
}

fn run_repl() {
  let assert Ok(user_input) = input(prompt: "$ ")
  let parts = string.split(user_input, " ")

  case parts {
    [] -> run_repl()
    [command, ..args] -> handle_command(command, args)
  }

  run_repl()
}

fn handle_command(command: String, args: List(String)) {
  case command {
    "exit" -> {
      let code =
        args
        |> list.first()
        |> result.try(int.parse)
        |> result.unwrap(0)

      exit(code)
    }
    "echo" -> {
      io.println(string.join(args, " "))
    }
    _ -> print_not_found_message(command)
  }
}

fn print_not_found_message(command: String) {
  io.println(command <> not_found_command)
}
