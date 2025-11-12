defmodule InputParser do
  @doc """
  Parses shell command with arguments and redirections.

  Returns a keyword list with:
  - cmd: command name
  - args: list of arguments
  - stdout: {file, bool} (if stdout redirect exists)
  - stderr: {file, bool} (if stderr redirect exists)

  Examples:
    iex> parse("cat 1.txt invalid 1> result.txt 2> errors.txt")
    [
      cmd: "cat",
      args: ["1.txt", "invalid"],
      stdout: {"result.txt", false},
      stderr: {"errors.txt", false}
    ]

    iex> parse("ls >> output.log")
    [cmd: "ls", args: [], stdout: {"output.log", true}]

    iex> parse("")
    []
  """
  def parse(""), do: []

  def parse(user_input) do
    # Match all redirections: 1>, 1>>, 2>, 2>>, >, >>
    redirect_regex = ~r/(1|2|)(>>|>)\s*(\S+)/

    redirects =
      Regex.scan(redirect_regex, user_input)
      |> Enum.reduce([], fn [_full, fd, op, file], acc ->
        context =
          case fd do
            "1" -> :stdout
            "2" -> :stderr
            "" -> :stdout
          end

        Keyword.put(acc, context, {file, op == ">>"})
      end)

    # Remove all redirections from command
    command_part =
      Regex.replace(redirect_regex, user_input, "")
      |> String.trim()

    # Parse command and arguments
    case String.split(command_part) |> Enum.reject(&(&1 == "")) do
      [] ->
        []

      [cmd | args] ->
        [cmd: cmd, args: args] ++ redirects
    end
  end
end
