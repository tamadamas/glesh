defmodule CLI do
  @builtins ["exit", "echo", "type", "pwd", "cd"]

  def main(_args) do
    run_loop()
  end

  defp run_loop do
    user_input =
      IO.gets("$ ")
      |> String.trim()
      |> InputParser.parse()

    if Keyword.has_key?(user_input, :cmd) do
      execute(user_input)
    end

    run_loop()
  end

  def execute(user_input) do
    cmd = Keyword.get(user_input, :cmd)
    args = Keyword.get(user_input, :args)

    out_device = init_device(user_input, :stdout)
    err_device = init_device(user_input, :stderr)

    try do
      case run_command(cmd, args) do
        :ok ->
          :ok

        {:ok, result} ->
          write_output(result, out_device)

        {:error, message} ->
          write_output(message, out_device)

        {success, error} ->
          write_output(success, out_device, no_newline: true)
          write_output(error, err_device, no_newline: true)
      end
    after
      !is_atom(out_device) && File.close(out_device)
      !is_atom(err_device) && File.close(err_device)
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
    message =
      args
      |> Enum.join(" ")
      |> strip_quotes

    {:ok, message}
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

  def run_command("cd", ["."]) do
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
        {_, %Rambo{out: result, err: error}} = Rambo.run(path, args, log: false)

        {result, error}

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

  def write_output(output, device \\ :stdio, opts \\ []) do
    IO.write(
      device,
      if Keyword.has_key?(opts, :no_newline) do
        output
      else
        [output, ?\n]
      end
    )
  end

  defp strip_quotes(arg) do
    Enum.find_value(["\"", "'"], arg, fn quote ->
      if String.starts_with?(arg, quote) and String.ends_with?(arg, quote) do
        String.slice(arg, 1..-2//1)
      end
    end)
  end

  # context = :stdout || :stderr
  defp init_device(user_input, context) do
    cond do
      Keyword.has_key?(user_input, context) ->
        {path, append} = Keyword.get(user_input, context)

        {:ok, file} =
          if append do
            File.open(path, [:append, :utf8])
          else
            File.open(path, [:write, :utf8])
          end

        file

      true ->
        case context do
          :stdout -> :stdio
          :stderr -> :stderr
        end
    end
  end
end
