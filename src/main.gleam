import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import gleave.{exit}
import input.{input}

pub fn main() {
  run_loop()
}

fn run_loop() {
  let assert Ok(user_input) = input(prompt: "$ ")
  let parts = string.split(user_input, " ")

  case parts {
    [] -> run_loop()
    [command, ..args] -> run_command(command, args)
  }

  run_loop()
}

fn run_command(command: String, args: List(String)) {
  case command {
    "exit" -> run_exit_command(args)
    "echo" -> run_echo_command(args)
    "type" -> run_type_command(args)
    _ -> io.println(command <> ": command not found")
  }
}

fn run_exit_command(args: List(String)) {
  case args {
    [] -> exit(0)
    _ -> {
      args
      |> list.first()
      |> result.try(int.parse)
      |> result.unwrap(1)
      |> exit
    }
  }
}

fn run_echo_command(args: List(String)) {
  io.println(string.join(args, " "))
}

fn run_type_command(args: List(String)) {
  let target = result.unwrap(list.first(args), "")
  let message = case target {
    "exit" | "echo" | "type" -> " is a shell builtin"
    _ -> ": not found"
  }

  io.println(target <> message)
}
