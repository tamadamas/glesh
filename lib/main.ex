defmodule CLI do
  def main(_args) do
    run_loop()
  end

  defp run_loop do
    user_input = IO.gets("$ ") |> String.trim_trailing
    
    IO.puts(user_input <> ": command not found")

    run_loop()
  end
end
