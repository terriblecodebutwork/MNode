defmodule Bex.QuickApi do
  use GenServer
  require Logger

  # Client

  def start_link(arg) do
    GenServer.start_link(__MODULE__, arg, name: __MODULE__)
  end

  # Server (callbacks)

  def init(_) do
    {:ok, %{ip: nil, ref: nil, port: nil, rejected: []}, {:continue, :open_port}}
  end

  def handle_continue(:open_port, state) do
    state |> open_and_monitor_quickapi()
  end

  def handle_info({:DOWN, ref, :port, port, _}, %{port: port, ref: ref} = state) do
    state |> open_and_monitor_quickapi()
  end

  def handle_info(
        {port, {:data, 'connection closed\n'}},
        %{ip: ip, port: port, ref: ref, rejected: rejected} = state
      ) do
    Port.demonitor(ref)
    Port.close(port)

    state
    |> Map.put(:rejected, [ip | rejected])
    |> open_and_monitor_quickapi()
  end

  def handle_info({_port, {:data, data}}, state) do
    Logger.debug("QuickApi: #{inspect(data)}")
    {:noreply, state}
  end

  def open_and_monitor_quickapi(state) do
    {port, ip} = start_quickapi(state.rejected)
    ref = Port.monitor(port)
    Logger.info("QuickApi started at port: #{inspect(port)}")
    {:noreply, %{state | ref: ref, port: port, ip: ip}}
  end

  def start_quickapi(rejected) do
    ip = find_fastest_ip(rejected)

    {Port.open(
       {:spawn_executable, "./run.sh"},
       args: ["quickapi", "-p", ip]
     ), ip}
  end

  def ping(host) do
    System.cmd("ping", ["-c", "5", host])
    |> elem(1)
  end

  def find_fastest_ip(rejected) do
    ips =
      :sv_peer.get_addrs_ipv4_dns()
      |> Enum.map(fn x -> ip_to_string(x) end)

    ips =
      case ips -- rejected do
        [] -> ips
        other -> other
      end

    pid = self()

    tasks =
      for ip <- ips do
        spawn(fn ->
          resp = ping(ip)
          send(pid, {:resp, resp, ip})
        end)
      end

    receive do
      {:resp, 0, ip} ->
        for pid <- tasks do
          Process.exit(pid, :kill)
        end

        ip

      _ ->
        :ok
    after
      5_000 ->
        {:error, :timeout}
    end
  end

  def ip_to_string({a, b, c, d}) do
    [a, b, c, d] |> Enum.join(".")
  end

  def ip_to_string({a, b, c, d, e, f}) do
    [a, b, c, d, e, f] |> Enum.join(".")
  end
end
