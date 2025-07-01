%%%-------------------------------------------------------------------
%%% @author mateu
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. cze 2025 17:49
%%%-------------------------------------------------------------------
-module(pingpong).
-author("mateu").

%% API
-export([start/0, stop/0, play/1, pong_loop/0, ping_loop/0]).


start() ->
  Pong = spawn(pingpong, pong_loop, []), % (modul, funkcja, arg)
  Ping = spawn(pingpong, ping_loop, []),
  register(pong, Pong),
  register(ping, Ping),
  ok.

stop() ->
  ping ! stop,
  pong ! stop.

play(N) when is_integer(N) ->
  ping ! {play, N}.

pong_loop() ->
  receive
    {pong, N} ->
      io:format("Pong ~p ~n", [N]),
      if
        N > 0 ->
          ping ! {play, N - 1},
          pong_loop();
        N =:= 0 ->
          io:format("Pong done.~n")
      end;
    stop ->
      io:format("Pong stopped! ~n")
  end.

ping_loop() ->
  receive
    {play, N} ->
      io:format("Ping ~p ~n", [N]),
      if
        N > 0 ->
          pong ! {pong, N - 1},
          ping_loop();
        N =:= 0 ->
          pong ! {pong, 0},
          io:format("Ping done.~n")
      end;
    stop ->
      io:format("Ping stopped!~n")
  end.