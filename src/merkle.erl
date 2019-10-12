-module(merkle).
-export([start/1, pairs/1]).

start(H) ->
  R = merkle_root(H),
  io:fwrite("~p~n", [bin_to_hex(reverse_binary(R))]).

transactions(225943) ->
  [<<16#fcd5ef8002e2f8197486da9198364179094b7183dd11940c6176ee1cda4f2ee3:256/little>>];
transactions(85544) ->
    [
        <<16#090d7de59fd8909ce6240ab80ef2c7b62249023ff73835889f0877d78d0481e6:256/little>>,
        <<16#6f9fc43904bdbf7023112ddfc7e787d781bdea9f4418ed1f089c5ebf47894797:256/little>>,
        <<16#c143dee928d2364f296e5a5ee8145dc39475c02f49a461d9c356a4c86d8e89e7:256/little>>
    ];
transactions(97624) ->
    [
        <<16#9531e98ead20973296ad1d23169dfa25c50fd6c5a9741771346b55233410e614:256/little>>,
        <<16#d831c29eb5d9158df4496ad467fe8dce717f4cda049ee89edde29a98002bb4ff:256/little>>
    ].

merkle_root(H) ->
  pairs_merkle(transactions(H)).

pairs_merkle([H]) ->
  H;
pairs_merkle(L) ->
  P = pairs(L),
  pairs_merkle(upper_hashes(P)).

pairs([H1, H2|T]) ->
  [[H1, H2]|pairs(T)];
pairs([H|_T]) ->
  [[H, H]];
pairs([]) ->
  [].

upper_hashes(L) ->
  [concat_hash(H) || H <- L].

concat_hash([H1, H2]) ->
  double256(<<H1/binary, H2/binary>>).

double256(B) ->
  crypto:hash(sha256, crypto:hash(sha256, B)).

bin_to_hex(Bin) ->
  lists:flatten([byte_to_hex(X) || X <- binary_to_list(Bin)]).

byte_to_hex(Byte) ->
  [int_to_hex(Byte div 16), int_to_hex(Byte rem 16)].

int_to_hex(Int) when Int < 10 -> $0 + Int;
int_to_hex(Int) when Int > 9 -> $A + Int - 10.

reverse_binary(B) ->
    X0 = binary:bin_to_list(B),
    X1 = lists:reverse(X0),
    binary:list_to_bin(X1).