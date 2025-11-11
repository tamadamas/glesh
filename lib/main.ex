defmodule CLI do
  @builtins ["exit", "echo", "type", "pwd", "cd"]

  def main(_args) do
    run_loop()
  end

  defp run_loop do
    user_input =
      IO.gets("$ ")
      |> String.trim()
      |> parse_args

    if Keyword.has_key?(user_input, :cmd) do
      eval_input(user_input)
    end

    run_loop()
  end

  def eval_input(user_input) do
    cmd = Keyword.get(user_input, :cmd)
    args = Keyword.get(user_input, :args)

    case run_command(cmd, args) do
      :ok ->
        :ok

      {:ok, result} ->
        IO.puts(result)

      {:error, message} ->
        IO.puts(message)

      {:ok, result, opts} ->
        IO.write(result)

        unless Keyword.has_key?(opts, :no_newline) do
          IO.write("\n")
        end

      {:error, message, opts} ->
        IO.write(:stderr, message)

        unless Keyword.has_key?(opts, :no_newline) do
          IO.write("\n")
        end
    end
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

  defp parse_args("") do
    []
  end

  defp parse_args(user_input) do
    {command_part, redirects} = parse_redirects(user_input)

    case String.split(command_part, " ") |> Enum.map(&String.trim/1) do
      [] ->
        []

      [cmd | args] ->
        Keyword.merge(redirects, cmd: cmd, args: args)
    end
  end

  defp parse_redirects(user_input) do
    cond do
      String.contains?(user_input, ">>") -> split_redirect(user_input, ">>")
      String.contains?(user_input, ">") -> split_redirect(user_input, ">")
      true -> {user_input, []}
    end
  end

  defp split_redirect(input, delimiter) do
    [command, file] =
      String.split(input, delimiter, parts: 2)
      |> Enum.map(&String.trim/1)

    {command, [file: file, append: delimiter == ">>"]}
  end
end
