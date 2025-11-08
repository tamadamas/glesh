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

  defp run_command("exit", args) do
    case args do
      [] ->
        System.halt(0)

      _ ->
        args
        |> List.first()
        |> String.to_integer()
        |> System.halt()
    end
  rescue
    ArgumentError -> System.halt(1)
  end

  defp run_command("echo", args) do
    IO.puts(Enum.join(args, " "))
  end

  defp run_command("type", args) do
    target = List.first(args, "")

    message =
      case target do
        "" ->
          "type: <arg> is required"

        builtin when builtin in ["exit", "echo", "type"] ->
          " is a shell builtin"

        _ ->
          case lookup_path(target) do
            {:ok, path} -> " is #{path}"
            {:error} -> ": not found"
          end
      end

    IO.puts(target <> message)
  end

  defp run_command(command, _args) do
    IO.puts("#{command}: command not found")
  end

  defp lookup_path(command) do
    case System.find_executable(command) do
      nil -> {:error}
      path -> {:ok, path}
    end
  end
end
