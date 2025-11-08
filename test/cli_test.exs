defmodule CLITest do
  use ExUnit.Case, async: true
  import ExUnit.CaptureIO

  IO.puts("CLITest module is loading!")

  describe "run_command/2 - exit" do
    test "exits with code 0 when no args provided" do
      assert catch_exit(CLI.run_command("exit", [])) == {:shutdown, 0}
    end

    test "exits with specified code when valid integer provided" do
      assert catch_exit(CLI.run_command("exit", ["5"])) == {:shutdown, 5}
    end

    test "exits with code 1 when invalid argument provided" do
      assert catch_exit(CLI.run_command("exit", ["invalid"])) == {:shutdown, 1}
    end

    test "uses first argument when multiple args provided" do
      assert catch_exit(CLI.run_command("exit", ["3", "ignored"])) == {:shutdown, 3}
    end
  end

  describe "run_command/2 - echo" do
    test "prints empty line when no args provided" do
      output = capture_io(fn -> CLI.run_command("echo", []) end)
      assert output == "\n"
    end

    test "prints single argument" do
      output = capture_io(fn -> CLI.run_command("echo", ["hello"]) end)
      assert output == "hello\n"
    end

    test "preserves argument order" do
      output = capture_io(fn -> CLI.run_command("echo", ["first", "second", "third"]) end)
      assert output == "first second sfsfthird\n"
    end
  end

  describe "run_command/2 - type" do
    test "shows error when no args provided" do
      output = capture_io(fn -> CLI.run_command("type", []) end)
      assert output == "type: <arg> is required\n"
    end

    test "identifies exit as builtin" do
      output = capture_io(fn -> CLI.run_command("type", ["exit"]) end)
      assert output == "exit is a shell builtin\n"
    end

    test "identifies echo as builtin" do
      output = capture_io(fn -> CLI.run_command("type", ["echo"]) end)
      assert output == "echo is a shell builtin\n"
    end

    test "identifies type as builtin" do
      output = capture_io(fn -> CLI.run_command("type", ["type"]) end)
      assert output == "type is a shell builtin\n"
    end

    test "identifies pwd as builtin" do
      output = capture_io(fn -> CLI.run_command("type", ["pwd"]) end)
      assert output == "pwd is a shell builtin\n"
    end

    test "finds executable in PATH" do
      output = capture_io(fn -> CLI.run_command("type", ["ls"]) end)
      assert output =~ ~r/ls is \/.*\/ls\n/
    end

    test "reports not found for non-existent command" do
      output = capture_io(fn -> CLI.run_command("type", ["nonexistentcommand123"]) end)
      assert output == "nonexistentcommand123: not found\n"
    end

    test "ignores extra arguments" do
      output = capture_io(fn -> CLI.run_command("type", ["echo", "extra", "args"]) end)
      assert output == "echo is a shell builtin\n"
    end
  end

  describe "run_command/2 - external commands" do
    test "shows error for non-existent command" do
      output = capture_io(fn -> CLI.run_command("nonexistentcmd789", []) end)
      assert output == "nonexistentcmd789: command not found\n"
    end

    test "passes arguments to external command" do
      # Using 'printf' as it's commonly available
      output = capture_io(fn -> CLI.run_command("printf", ["hello"]) end)
      assert output == "hello"
    end
  end

  describe "lookup_path/1" do
    test "returns ok tuple with path for existing command" do
      assert {:ok, path} = CLI.lookup_path("ls")
      assert String.ends_with?(path, "ls")
    end

    test "returns error tuple for non-existent command" do
      assert {:error} = CLI.lookup_path("thiscommanddoesnotexist999")
    end
  end
end
