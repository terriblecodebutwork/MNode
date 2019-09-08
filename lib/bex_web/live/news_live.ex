defmodule BexWeb.NewsLive do
  use Phoenix.LiveView

  def render(assigns) do
    ~L"""
    <h1>News</h1>
    <script src="https://www.moneybutton.com/moneybutton.js"></script>
    <div id="my-money-button"></div>
    <script>
      const div = document.getElementById('my-money-button')
      moneyButton.render(div, {
        to: "390@moneybutton.com",
        editable: true,
        currency: "BSV"
      })
    </script>
    """
  end

  def mount(_session, socket) do

    {:ok, socket}
  end
end