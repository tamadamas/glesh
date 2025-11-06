import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import gleave.{exit}
import input.{input}

const not_found_command = ": command not found"

const commands = ["exit", "echo", "type"]

pub fn main() {
  run_loop()
}

fn run_loop() {
  let assert Ok(user_input) = input(prompt: "$ ")
  let parts = string.split(user_input, " ")

  case parts {
    [] -> run_loop()
    [command, ..args] -> handle_command(command, args)
  }

  run_loop()
}

fn handle_command(command: String, args: List(String)) {
  case command {
    "exit" -> {
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
    "echo" -> io.println(string.join(args, " "))
    "type" -> {
      let target = result.unwrap(list.first(args), "")
      let found = list.find(commands, fn(x) { x == target })

      let message = case found {
        Ok(_) -> ": is a shell builtin"
        Error(_) -> ": not found"
      }

      io.println(target <> message)
    }
    _ -> print_not_found_message(command)
  }
}

fn print_not_found_message(command: String) {
  io.println(command <> not_found_command)
}
