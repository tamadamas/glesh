import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/set
import gleam/string
import glexec as exec
import input.{input}
import simplifile
import system

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
    _ -> {
      case run_external_command(command, args) {
        True -> io.println("Done.")
        _ -> io.println(command <> ": command not found")
      }
    }
  }
}

fn run_exit_command(args: List(String)) {
  case args {
    [] -> system.exit(0)
    _ -> {
      args
      |> list.first()
      |> result.try(int.parse)
      |> result.unwrap(1)
      |> system.exit
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
    _ -> {
      case lookup_path(target) {
        Ok(path) -> " is " <> path
        Error(_) -> ": not found"
      }
    }
  }

  io.println(target <> message)
}

fn run_external_command(command: String, args: List(String)) -> Bool {
  let print_output = fn(output: exec.Output) {
    case output {
      exec.Output(result) -> {
        list.map(result, fn(item) {
          case item {
            exec.Stdout(list) -> io.println(string.join(list, "\n"))
            exec.Stderr(list) -> io.println_error(string.join(list, "\n"))
          }
        })
      }
    }
  }

  case lookup_path(command) {
    Ok(command_path) -> {
      let result =
        exec.new()
        |> exec.with_stdout(exec.StdoutCapture)
        |> exec.run_sync(exec.Execve([command_path, ..args]))

      case result {
        Ok(output) -> print_output(output)
        Error(error) -> print_output(error.output)
      }

      True
    }
    _ -> False
  }
}

fn lookup_path(command: String) -> Result(String, Nil) {
  list.find_map(system.get_path_dirs(), fn(dir) {
    case simplifile.is_directory(dir) {
      Ok(True) -> {
        dir
        |> system.path_join(command)
        |> lookup_command_path
      }
      _ -> Error(Nil)
    }
  })
}

fn lookup_command_path(command_path: String) -> Result(String, Nil) {
  case simplifile.file_info(command_path) {
    Ok(file_info) -> {
      let perm = simplifile.file_info_permissions(file_info)

      case set.contains(perm.user, simplifile.Execute) {
        True -> Ok(command_path)
        _ -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}
