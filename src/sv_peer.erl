-module(sv_peer).

% -liciense("MIT")
% -author("terriblecodebutwork aka. Jay Zhang")
% -email("bitcoinsv@yahoo.com")
% -paymail("390@moneybutton.com")

-compile([export_all]).
-define(MAGIC, 16#E3E1F3E8).
-define(PROTOCOL_VERSION, 31800).
-define(GENESIS, <<111, 226, 140, 10, 182, 241, 179, 114, 193, 166, 162, 70, 174, 99, 247, 79,
  147, 30, 131, 101, 225, 90, 8, 156, 104, 214, 25, 0, 0, 0, 0, 0>>).

-record(peer, {state = start, host, port, socket, buffer}).

connect(Host) ->
    spawn(fun() -> do_connect(Host) end).

do_connect(Host) ->
    case gen_tcp:connect(Host, 8333, [binary, {packet, 0}, {active, false}]) of
        {ok, Socket} ->
            loop(#peer{socket=Socket});
        _ ->
            ok
    end.

loop(#peer{state = start} = P) ->
    send_message(P#peer.socket, version_msg()),
    loop(P#peer{state = version_sent});

loop(#peer{state = version_sent} = P) ->
    {ok, B} = gen_tcp:recv(P#peer.socket, 0, 50*1000),
    loop(P#peer{state = loop, buffer = B});

loop(#peer{state = loop, socket = Socket} = P) ->
    receive
        version ->
            send_message(Socket, version_msg());
        ping ->
            send_message(Socket, ping_msg());
        getheaders ->
            send_message(Socket, getheaders_msg([?GENESIS]));
        getaddr ->
            send_message(Socket, getaddr_msg());
        mempool ->
            send_message(Socket, mempool_msg());
        {tx, Tx} ->
            send_message(Socket, tx_msg(Tx));
        close ->
            gen_tcp:close(Socket),
            exit(normal);
        _Other ->
            invalid_msg
    after 0 ->
        ok
    end,
    case parse_message(P#peer.buffer) of
        {ok, Command, Payload} ->
            {Data, _Rest} = parse(Command, Payload),
            % io:format("[remote] ~s\n~p\n", [Command, Data]),
            handle_command(Command, Data, Socket),
            loop(P#peer{buffer = <<>>});
        {error, incomplete} ->
            {ok, B} = gen_tcp:recv(Socket, 0, 120*1000),
            Buffer = P#peer.buffer,
            loop(P#peer{buffer = <<Buffer/bytes, B/bytes>>})
    end.


send_message(Socket, Msg) ->
    {ok, Command, P} = parse_message(Msg),
    {Data, _Rest} = parse(Command, P),
    % io:format("[local ] ~s\n~p\n", [Command, Data]),
    gen_tcp:send(Socket, Msg).

get_checksum(Bin) ->
    <<C:4/bytes, _/bytes>> = double_hash256(Bin),
    C.

double_hash256(Bin) ->
    crypto:hash(sha256, crypto:hash(sha256, Bin)).

%% make messages

version_msg() ->
    Services = <<1, 0:(7*8)>>,
    Timestamp = get_timestamp(),
    Addr_recv = <<Services/binary, 0:(10*8), 16#FF, 16#FF, 0, 0, 0, 0, 0, 0>>,
    Addr_from = <<Services/binary, 0:(10*8), 16#FF, 16#FF, 0, 0, 0, 0, 0, 0>>,
    Nonce = crypto:strong_rand_bytes(8),
    User_agent = varstr(<<"\r/IS THIS A VALID USER AGNET?/">>),
    Strat_height = 0,
    Payload = <<?PROTOCOL_VERSION:32/little,
                Services/binary,
                Timestamp:64/little,
                Addr_recv/binary,
                Addr_from/binary,
                Nonce/binary,
                User_agent/binary,
                Strat_height:32/little
              >>,
    make_message(version, Payload).

verack_msg() ->
    make_message(verack, <<>>).

ping_msg() ->
    make_message(ping, <<>>).

pong_msg(Bin) ->
    make_message(pong, Bin).

getdata_msg(Bin) ->
    make_message(getdata, Bin).

getheaders_msg(Locators) ->
    N = varint(length(Locators)),
    HL = << <<Hash/bytes>> || Hash <- Locators >>,
    make_message(getheaders, <<?PROTOCOL_VERSION:32/little, N/bytes, HL/bytes, 0:(32*8)>>).

getaddr_msg() ->
    make_message(getaddr, <<>>).

mempool_msg() ->
    make_message(mempool, <<>>).

tx_msg(Tx) ->
    make_message(tx, Tx).

atom_to_cmd(A) ->
    S = list_to_binary(atom_to_list(A)),
    L = byte_size(S),
    <<S/binary, 0:((12-L)*8)>>.

make_message(Command, Payload) ->
    Size = byte_size(Payload),
    Checksum = get_checksum(Payload),
    CommandBin = atom_to_cmd(Command),
    <<?MAGIC:32/big, CommandBin/binary, Size:32/little,
      Checksum/binary, Payload/binary>>.

get_timestamp() ->
    {A, B, _} = os:timestamp(),
    A*1000000 + B.

varstr(Bin) ->
    Len = varint(byte_size(Bin)),
    <<Len/binary, Bin/binary>>.

varint(X) when X < 16#fd -> <<X>>;
varint(X) when X =< 16#ffff  -> <<16#fd, X:16/little>>;
varint(X) when X =< 16#ffffffff  -> <<16#fe, X:32/little>>;
varint(X) when X =< 16#ffffffffffffffff  -> <<16#ff, X:64/little>>.


%% parsing


parse_message(<<?MAGIC:32/big, Command:12/bytes, Size:32/little-integer,
          Checksum:4/bytes, Payload:Size/bytes, _Rest/bytes>>) ->
    Checksum = get_checksum(Payload),
    {ok, cmd_to_list(Command), Payload};

parse_message(_B) ->
    {error, incomplete}.

cmd_to_list(B) ->
    L = binary_to_list(B),
    string:strip(L, right, 0).

parse("version", <<Version:32/little,
                   Services:8/binary,
                   Timestamp:64/little,
                   Addr_recv:26/binary,
                   Addr_from:26/binary,
                   Nonce:8/binary,
                   Rest/binary
                 >>) ->
    {User_agent, Rest1} = parse_varstr(Rest),
    <<Strat_height:32/little, Rest2/binary>> = Rest1,
    {#{version => Version,
            services => Services,
            timestamp => Timestamp,
            addr_recv => parse_addr(Addr_recv),
            addr_from => parse_addr(Addr_from),
            nonce => Nonce,
            user_agent => User_agent,
            start_height => Strat_height}, Rest2};

parse("verack", <<>>) ->
    {<<>>, <<>>};

parse("ping", Payload) ->
    {Payload, <<>>};

parse("pong", Payload) ->
    {Payload, <<>>};

parse("getheaders", <<Version:32/little, Bin/binary>>) ->
    {L, Rest} = parse_varint(Bin),
    Size = 32*L,
    <<HL:Size/bytes, SH:32/bytes, Rest2/bytes>> = Rest,
    {#{version => Version,
           hash_count => L,
           block_locator_hashes => [ X || <<X:32/binary>> <= HL],
           hash_stop => SH}, Rest2};

parse("inv", Bin) ->
    {L, Rest} = parse_varint(Bin),
    Size = 36*L,
    <<INV:Size/bytes, Rest2/bytes>> = Rest,
    {#{raw => Bin, invs => [ parse_inv(X) || <<X:36/bytes>> <= INV]}, Rest2};

parse("headers", Bin) ->
    {L, Rest} = parse_varint(Bin),
    Size = 81*L,
    <<HL:Size/bytes, Rest2/bytes>> = Rest,
    {#{headers => [ parse_header(X) || <<X:81/bytes>> <= HL]}, Rest2};

parse("tx", Raw = <<Ver:32/little-signed, B/bytes>>) ->
    % io:format("transaction: ~s~n", [io_lib:print(Raw)]),
    {TX_in_count, R1} = parse_varint(B),
    {TX_in, R2} = parse_tx_in(R1, [], TX_in_count),
    {TX_out_count, R3} = parse_varint(R2),
    {TX_out, R4} = parse_tx_out(R3, [], TX_out_count, 0),
    <<Lock_time:32/little, Rest/bytes>> = R4,
    {#{version => Ver, input => TX_in,
      output => TX_out, lock_time => Lock_time,
      txid => to_rpc_hex(double_hash256(Raw))}, Rest};

parse("addr", Bin) ->
    {L, Rest} = parse_varint(Bin),
    Size = 30*L,
    <<AL:Size/bytes, Rest2/bytes>> = Rest,
    {#{addrs => [ parse_addr(X) || <<X:30/bytes>> <= AL]}, Rest2};

parse("getdata", Bin) ->
    parse("inv", Bin);

parse("block", Bin) ->
    {parse_header(Bin), <<>>};

parse(Other, Bin) ->
    % io:format("=====================~s~n", [io_lib:print({Other, Bin})]),
    {Bin, <<>>}.

% parse("alert", <<Ver:32/little-signed,
%                  Until:64/little-signed,
%                  Expir:64/little-signed,
%                  ID:32/little-signed,
%                  Cancel:32/little-signed,
%                  B1/bytes>>) ->
%     {LCancel, B2} = parse_varint(B1),
%     {Cancels, B3} = parse_int32_set(B2, [], LCancel),
%     <<MinVer:32/little-signed, MaxVer:32/little-signed, B4/bytes>> = B3,
%     {LSubVer, B5} = parse_varint(B4),
%     {SubVers, B6} = parse_string_set(B5, [], LSubVer),
%     <<Priority:32/little-signed, B7/bytes>> = B6,
%     {Comment, B8} = parse_varstr(B7),
%     {StatusBar, B9} = parse_varstr(B8),
%     {RPCError, _B10} = parse_varstr(B9),
%     {ok, #{version => Ver,
%            until => Until,
%            expir => Expir,
%            id => ID,
%            cancel => Cancel,
%            cancel_set => Cancels,
%            min_ver => MinVer,
%            max_ver => MaxVer,
%            subver_set => SubVers,
%            priority => Priority,
%            comment => Comment,
%            status_bar => StatusBar,
%            rpc_error => RPCError}};


parse_tx_in(Rest, R, 0) ->
    {lists:reverse(R), Rest};

parse_tx_in(<<0:(32*8), Index:32/little, P/bytes>>, R, N) when N > 0 ->
    {L, R1} = parse_varint(P),
    <<Script:L/bytes, Seq:32/little, Rest/bytes>> = R1,
    parse_tx_in(Rest, [#{tx_ref => <<0:(32*8)>>, txid => to_rpc_hex(<<0:(32*8)>>), index => Index, raw_script => Script, hex_script => bin_to_hex(Script) ,coinbase => true,
                         script => bin_to_chars(Script), sequence => Seq}|R], N - 1);

parse_tx_in(<<TX_ref:32/bytes, Index:32/little, P/bytes>>, R, N) when N > 0 ->
    {L, R1} = parse_varint(P),
    <<Script:L/bytes, Seq:32/little, Rest/bytes>> = R1,
    parse_tx_in(Rest, [#{tx_ref => TX_ref, txid => to_rpc_hex(TX_ref), index => Index, raw_script => Script, hex_script => bin_to_hex(Script), coinbase => false,
                         script => parse_script(Script), sequence => Seq}|R], N - 1).

parse_script(S) ->
    try 'Elixir.BexLib.Script':parse(S) of
        R -> R
    catch
        _:_ -> "Invalid Script"
    end.

% parse_script(<<H:8/little, T/bytes>>, R) ->
%     case opcode(H, T) of
%         {Result, T1} ->
%             parse_script(T1, [Result | R]);
%         else ->
%             {else, lists:reverse(R)};
%         endif ->
%             {endif, lists:reverse(R)}
%     end;
% parse_script(<<>>, R) ->
%     lists:reverse(R).

%%FIXME there is a bug in script parse, use BexLib.Script.parse instead.
opcode(H, Bin) when H >= 0 andalso H =< 75 ->
    <<B:H/binary, R/bytes>> = Bin,
    {{"PUSH", B}, R};
opcode(99, Bin) ->
    {Left, Right, Bin1} = cond_clause(Bin, []),
    {{"IF", Left, Right}, Bin1};
opcode(100, Bin) ->
    {Left, Right, Bin1} = cond_clause(Bin, []),
    {{"IF", Right, Left}, Bin1};
% opcode(103, _Bin) ->
%     % ELSE
%     else;
% opcode(104, _Bin) ->
%     % ENDIF
%     endif;
opcode(106, Bin) ->
    {{"RETURN", Bin}, <<>>};
opcode(108, Bin) ->
    {"FROMALTSTACK", Bin};
opcode(111, Bin) ->
    {"3DUP", Bin};
opcode(112, Bin) ->
    {"2OVER", Bin};
opcode(115, Bin) ->
    {"IFDUP", Bin};
opcode(118, Bin) ->
    {"DUP", Bin};
opcode(135, Bin) ->
    {"EQUAL", Bin};
opcode(136, Bin) ->
    {"EQUALVERIFY", Bin};
opcode(169, Bin) ->
    {"HASH160", Bin};
opcode(172, Bin) ->
    {"CHECKSIG", Bin};
opcode(H, Bin) ->
    unkonw = H,
    {{H, Bin}, <<>>}.

cond_clause(<<H:8/little, T/bytes>>, {L, R, Rest}) ->
    case H of
        99 ->
            % IF
            cond_clause(T, 1);
        103 ->
            % ELSE
            ok;

        104 ->
            % ENDIF
            ok
    end,
    {L, R, Rest}.


parse_tx_out(Rest, R, 0, _I) ->
    {lists:reverse(R), Rest};

parse_tx_out(<<Value:64/little, P/bytes>>, R, N, I) when N > 0 ->
    {L, R1} = parse_varint(P),
    <<PK_script:L/bytes, Rest/bytes>> = R1,
    parse_tx_out(Rest, [#{index => I, value => Value, raw_script => PK_script, hex_script => bin_to_hex(PK_script), script => parse_script(PK_script)}|R], N - 1, I + 1).

parse_addr(<<Time:32/little, Services:8/bytes, IP:16/bytes, Port:16/little>>) ->
    #{timestamp => Time, services => Services, ip => IP, port => Port};
parse_addr(<<Services:8/bytes, IP:16/bytes, Port:16/little>>) ->
    #{services => Services, ip => IP, port => Port}.

% parse_int32_set(Rest, R, 0) ->
%     {lists:reverse(R), Rest};

% parse_int32_set(<<H:32/little-signed, Rest/bytes>>, R, N) when N > 0 ->
%     parse_int32_set(Rest, [H | R], N - 1).

% parse_string_set(Rest, R, 0) ->
%     {lists:reverse(R), Rest};

% parse_string_set(Bytes, R, N) when N > 0 ->
%     {Data, Rest} = parse_varstr(Bytes),
%     parse_int32_set(Rest, [Data | R], N - 1).


parse_inv(<<0:32/little, _H/binary>>) -> error;
parse_inv(<<1:32/little, H:32/bytes>>) -> {tx, H};
parse_inv(<<2:32/little, H:32/bytes>>) -> {block, H};
parse_inv(<<3:32/little, H:32/bytes>>) -> {filtered_block, H};
parse_inv(<<4:32/little, H:32/bytes>>) -> {cmpct_block, H}.

parse_header(<<Head:80/bytes, Rest/binary>>) ->
    <<Version:32/signed-little-integer,
               Prev_block:32/bytes,
               Merkle_root:32/bytes,
               Timestamp:32/little-integer,
               Bits:32/little-integer,
               Nonce:4/bytes>> = Head,
    {Tx_count, Rest2} = parse_varint(Rest),
    #{version => Version,
      prev_block => Prev_block,
      merkel_root => Merkle_root,
      timestamp => Timestamp,
      bits => decode_bits(Bits),
      nonce => Nonce,
      tx_count => Tx_count,
      hash => double_hash256(Head),
      txs => parse_txs(Rest2, [], Tx_count)}.

parse_txs(_Bin, R, 0) ->
    lists:reverse(R);
parse_txs(Bin, R, N) ->
    {Tx, Rest} = parse("tx", Bin),
    parse_txs(Rest, [Tx | R], N - 1).


parse_varstr(Bin) ->
    {Len, B} = parse_varint(Bin),
    << Data:Len/bytes, Rest/binary >> = B,
    {Data, Rest}.

parse_varint(<<16#fd, X:16/little, Rest/binary>>) -> {X, Rest};
parse_varint(<<16#fe, X:32/little, Rest/binary>>) -> {X, Rest};
parse_varint(<<16#ff, X:64/little, Rest/binary>>) -> {X, Rest};
parse_varint(<<X:8, Rest/binary>>) -> {X, Rest}.

decode_bits(Bits) when is_integer(Bits) ->
    decode_bits(binary:encode_unsigned(Bits));
decode_bits(<<N, D/bytes>>) ->
    L = byte_size(D),
    <<D/bytes, 0:((N-L)*8)>>.

% helper

bin_to_chars(Bin) ->
    io_lib:write_string(binary_to_list(Bin)).

trim_trailing(Bin) ->
    trim_trailing(Bin, 0).

trim_trailing(Bin, Byte) when is_binary(Bin) and is_integer(Byte) ->
    do_trim_trailing(rev(Bin), Byte).

do_trim_trailing(<< Byte, Bin/binary >>, Byte) ->
    do_trim_trailing(Bin, Byte);
do_trim_trailing(<< Bin/binary >>, _Byte) ->
    rev(Bin).

bin_to_hex(B) ->
    bin_to_hex(B, "").

bin_to_hex(B, D) ->
    binary:list_to_bin(string:join([io_lib:format("~2.16.0b", [X]) || <<X>> <= B ], D)).

digit(X) when X >= $0, X =< $9 ->
    X - $0;

digit(X) when X >= $a, X =< $z ->
    X - $a + 10;

digit(X) when X >= $A, X =< $Z ->
    X - $A + 10.

hex_to_bin(S) ->
    binary:list_to_bin(hex_to_bin(binary:bin_to_list(S), [])).

hex_to_bin([], R) ->
    lists:reverse(R);

hex_to_bin([$\  | T], R) ->
    hex_to_bin(T, R);

hex_to_bin([A, B | T], R) ->
    hex_to_bin(T, [digit(A)*16+digit(B)|R]).

rev(Binary) ->
   Size = erlang:size(Binary)*8,
   <<X:Size/integer-little>> = Binary,
   <<X:Size/integer-big>>.

to_rpc_hex(B) ->
    bin_to_hex(rev(B)).

from_rpc_hex(H) ->
    rev(hex_to_bin(H)).


%% validation

valid_pow(Hash, Target) ->
    rev(Hash) < Target.


%% peer behavior

handle_command("version", _Data, Socket) ->
    send_message(Socket, verack_msg());

handle_command("ping", Data, Socket) ->
    send_message(Socket, pong_msg(Data));

handle_command("getheaders", _Data, _Socket) ->
    ok;

handle_command("inv", #{raw := Bin}, Socket) ->
    send_message(Socket, getdata_msg(Bin));

handle_command(_Command, _Data, _Socket) ->
    ok.

%% other

block_locator(B) ->
    block_locator(B, [], 0, 1).

block_locator(?GENESIS, Re, _, _) ->
    lists:reverse(Re);
block_locator(B, Re, Len, Step) ->
    Pb = prev_block(B, Step),
    Len1 = Len + 1,
    Step1 = case Len1 > 10 of
                true ->
                    Step * 2;
                false ->
                    Step
            end,
    block_locator(Pb, [Pb | Re], Len1, Step1).

prev_block(B, 0) -> B;
prev_block(B, N) ->
    prev_block(get_prev(B), N-1).

get_prev(_B) -> todo.

%% :sv_peer.connect {139, 59, 67, 18}
%% :sv_peer.connect {159, 203, 171, 73}
%% :sv_peer.connect {159, 65, 152, 200}


%% @doc Get addrs for bootstrap from DNS.
get_addrs_ipv4_dns() ->
    L = ["seed.bitcoinsv.io",
         "seed.cascharia.com",
         "seed.satoshisvision.network"
        ],
    lists:flatten([nslookup_ipv4(A) || A <- L]).

nslookup_ipv4(Addr) ->
    Type = a,
    Class = in,
    inet_res:lookup(Addr, Class, Type).
