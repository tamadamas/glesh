defmodule CLI do
  @builtins ["exit", "echo", "type", "pwd", "cd"]

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

  def run_command("exit", args) do
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

  def run_command("echo", args) do
    IO.puts(Enum.join(args, " "))
  end

  def run_command("type", []) do
    IO.puts("type: <arg> is required")
  end

  def run_command("type", [target | _]) when target in @builtins do
    IO.puts("#{target} is a shell builtin")
  end

  def run_command("type", [target | _]) do
    message =
      case lookup_path(target) do
        {:ok, path} -> " is #{path}"
        {:error} -> ": not found"
      end

    IO.puts(target <> message)
  end

  def run_command("pwd", _args) do
    IO.puts(File.cwd!())
  end

  def run_command("cd", args) when args in [[], ["~"]] do
    File.cd!(System.user_home!())
  end

  def run_command("~", []) do
    File.cd!(System.user_home!())
  end

  def run_command("cd", ["."]) do
    :ok
  end

  def run_command("cd", [".."]) do
    File.cd!("..")
  end

  def run_command("..", []) do
    File.cd!("..")
  end

  def run_command("cd", [dir]) do
    case File.cd(dir) do
      :ok ->
        :ok

      {:error, _} ->
        IO.puts(:stderr, "cd: #{dir}: No such file or directory")
    end
  end

  def run_command(command, args) do
    case lookup_path(command) do
      {:ok, path} ->
        case System.cmd(path, args, arg0: command) do
          {output, 0} -> IO.write(output)
          {error, _} -> IO.write(:stderr, error)
        end

      {:error} ->
        IO.puts("#{command}: command not found")
    end
  end

  def lookup_path(command) do
    case System.find_executable(command) do
      nil -> {:error}
      path -> {:ok, path}
    end
  end
end
