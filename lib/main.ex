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
      [] ->
        run_loop()

      [command | args] ->
        case run_command(command, args) do
          :ok -> :ok
          {:ok, output} -> write_output(output)
          {:error, error} -> write_output(error, :stderr)
          {:ok, output, opts} -> write_output(output, :stdio, opts)
          {:error, error, opts} -> write_output(error, :stderr, opts)
        end
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
    {:ok, Enum.join(args, " ")}
  end

  def run_command("type", []) do
    {:error, "type: <arg> is required"}
  end

  def run_command("type", [target | _]) when target in @builtins do
    {:ok, "#{target} is a shell builtin"}
  end

  def run_command("type", [target | _]) do
    message =
      case lookup_path(target) do
        {:ok, path} -> " is #{path}"
        {:error, _} -> ": not found"
      end

    {:ok, target <> message}
  end

  def run_command("pwd", _args) do
    {:ok, File.cwd!()}
  end

  def run_command("cd", args) when args in [[], ["~"]] do
    File.cd!(System.user_home!())
    :ok
  end

  def run_command("~", []) do
    File.cd!(System.user_home!())
    :ok
  end

  def run_command("cd", ["."]) do
    :ok
  end

  def run_command("cd", [".."]) do
    File.cd!("..")
    :ok
  end

  def run_command("..", []) do
    File.cd!("..")
    :ok
  end

  def run_command("cd", [dir]) do
    case File.cd(dir) do
      :ok ->
        :ok

      {:error, _} ->
        {:error, "cd: #{dir}: No such file or directory"}
    end
  end

  def run_command(command, args) do
    case lookup_path(command) do
      {:ok, path} ->
        case System.cmd(path, args, arg0: command) do
          {output, 0} -> {:ok, output, [no_newline: true]}
          {error, _} -> {:error, error, [no_newline: true]}
        end

      {:error, error} ->
        {:error, error}
    end
  end

  def lookup_path(command) do
    case System.find_executable(command) do
      nil -> {:error, "#{command}: command not found"}
      path -> {:ok, path}
    end
  end

  def write_output(output, device \\ :stdio, options \\ []) do
    output =
      if Keyword.get(options, :no_newline, false) or String.ends_with?(output, "\n") do
        output
      else
        output <> "\n"
      end

    IO.write(device, output)
  end
end
