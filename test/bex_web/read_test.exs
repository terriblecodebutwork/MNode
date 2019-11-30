defmodule ReadTest do
  use ExUnit.Case
  alias BexWeb.ReadController

  describe "description_of_tests" do
    test "remove_prefix test" do
      assert ["19HxigV4QyBv3tHpQVcUEQyq1pzZVdoAut", "data", "type", "binary", "filename"] ==
               ReadController.remove_prefix([
                 "TimeSV.com",
                 "onchain_info",
                 "hash",
                 "|",
                 "19HxigV4QyBv3tHpQVcUEQyq1pzZVdoAut",
                 "data",
                 "type",
                 "binary",
                 "filename"
               ])
    end
  end
end
