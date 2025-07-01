%%%-------------------------------------------------------------------
%%% @author mateu
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. cze 2025 12:37
%%%-------------------------------------------------------------------
-module(myLists).
-author("mateu").

%% API
-export([contains/2, duplicateElements/1, sumFloats/1]).

contains([], _) ->
  false;

contains([X | _], X) ->
  true;

contains([_ | Tail], X) ->
  contains(Tail, X).

duplicateElements([]) ->
  [];

duplicateElements([H|T]) ->
  [H, H| duplicateElements((T))].

sumFloats(List) ->
  sumFloats(List, 0.0).

sumFloats([], Res) ->
  Res;

sumFloats([H|T], Res) when is_float(H) ->
  sumFloats(T, Res + H);

sumFloats([_|T], Res) ->
  sumFloats(T, Res).

