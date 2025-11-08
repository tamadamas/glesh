defmodule CLI do
  def main(_args) do
    run_loop()
  end

  defp run_loop do
    user_input =
      IO.gets("$ ")
      |> String.trim()
      |> String.split(" ")

    case user_input do
      [] -> run_loop()
      [command | args] -> run_command(command, args)
    end

    run_loop()
  end

  defp run_command(command, args) do
    case command do
      "exit" -> run_exit_command(args)
          "echo" -> run_echo_command(args)
      "type" -> run_type_command(args)
      _ -> IO.puts("#{command}: command not found")
    end
  end

  defp run_exit_command(args) do
    case args do
      [] ->
        System.halt(0)
      _ ->
        args
        |> List.first()
        |> String.to_integer()
        |> System.halt()
    end

    rescue ArgumentError -> System.halt(1)
  end

  defp run_echo_command(_args) do
  end

  defp run_type_command(_args) do
  end
end
