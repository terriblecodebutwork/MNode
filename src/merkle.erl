-module(merkle).
-export([start/1, pairs/1]).

start(H) ->
  R = merkle_root(H),
  print_hash(R).

print_hash(H) ->
  io:fwrite("~p~n", [bin_to_hex(reverse_binary(H))]).

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
    ];
transactions(100002) ->
    [
        <<16#ef1d870d24c85b89d92ad50f4631026f585d6a34e972eaf427475e5d60acf3a3:256/little>>,
        <<16#f9fc751cb7dc372406a9f8d738d5e6f8f63bab71986a39cf36ee70ee17036d07:256/little>>,
        <<16#db60fb93d736894ed0b86cb92548920a3fe8310dd19b0da7ad97e48725e1e12e:256/little>>,
        <<16#220ebc64e21abece964927322cba69180ed853bb187fbc6923bac7d010b9d87a:256/little>>,
        <<16#71b3dbaca67e9f9189dad3617138c19725ab541ef0b49c05a94913e9f28e3f4e:256/little>>,
        <<16#fe305e1ed08212d76161d853222048eea1f34af42ea0e197896a269fbf8dc2e0:256/little>>,
        <<16#21d2eb195736af2a40d42107e6abd59c97eb6cffd4a5a7a7709e86590ae61987:256/little>>,
        <<16#dd1fd2a6fc16404faf339881a90adbde7f4f728691ac62e8f168809cdfae1053:256/little>>,
        <<16#74d681e0e03bafa802c8aa084379aa98d9fcd632ddc2ed9782b586ec87451f20:256/little>>
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
  Hashes = [concat_hash(H) || H <- L],
  [print_hash(X) || X <- Hashes],
  Hashes.

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