%%%-------------------------------------------------------------------
%%% @author mateu
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. cze 2025 18:23
%%%-------------------------------------------------------------------
-module(qsort).
-author("mateu").

%% API
-export([less_than/2,  grt_eq_than/2, qs/1, random_elems/3]).

less_than(List, Arg) -> [X || X <- List, X < Arg].

grt_eq_than(List, Arg) -> [X || X <- List, X >= Arg].

qs([]) -> [];

qs([Pivot|Tail]) -> qs(less_than(Tail, Pivot)) ++ [Pivot] ++ qs(grt_eq_than(Tail, Pivot)).

random_elems(N, Min, Max) ->
  Range = Max - Min + 1,
  [Min - 1 + rand:uniform(Range) || _ <- lists:seq(1, N)].
