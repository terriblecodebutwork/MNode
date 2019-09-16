defmodule Bex.Util do

  def path_to_name(path) do
    parent =
      if String.contains?(path, "/") do
        Path.dirname(path)
      else
        false
      end

    name = path
    {parent, name}
  end

end