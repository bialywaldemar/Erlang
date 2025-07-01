%%%-------------------------------------------------------------------
%%% @author mateu
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. cze 2025 11:25
%%%-------------------------------------------------------------------
-module(pollution2).
-author("mateu").

-record(pomiar, {
  stacja,
  czas,
  wartosci
}).

%% API
-export([create_monitor/0, add_station/3, add_value/5, remove_value/4, get_one_value/4,
  get_station_min/3, get_daily_mean/3, get_sample_data/0]).

get_sample_data() ->
  P1 = #pomiar{
    stacja = "Krakow",
    czas = "2025-02-01 14:00",
    wartosci = [1.23, 31.45, 0.03]
  },
  P2 = #pomiar{
    stacja = "Bielsko",
    czas = "2025-03-04 12:00",
    wartosci = [1.01, 21.32, 0.06]
  },
  P3 = #pomiar{
    stacja = "Warszawa",
    czas = "2025-01-14 21:00",
    wartosci = [8.32, 20.0, 1.0]
  },
  [P1, P2, P3].

create_monitor() -> [].

add_station(Monitor, Station, Time) ->
  P = #pomiar{}.

add_value(_Arg0, _Arg1, _Arg2, _Arg3, _Arg4) ->
  erlang:error(not_implemented).



remove_value(_Arg0, _Arg1, _Arg2, _Arg3) ->
  erlang:error(not_implemented).

get_one_value(_Arg0, _Arg1, _Arg2, _Arg3) ->
  erlang:error(not_implemented).

get_station_min(_Arg0, _Arg1, _Arg2) ->
  erlang:error(not_implemented).

get_daily_mean(_Arg0, _Arg1, _Arg2) ->
  erlang:error(not_implemented).