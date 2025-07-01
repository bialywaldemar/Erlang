%%%-------------------------------------------------------------------
%%% @author mateu
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%% Pollution monitoring system
%%% @end
%%% Created : 28. cze 2025 11:25
%%%-------------------------------------------------------------------
-module(pollution2).
-author("mateu").

-record(pomiar, {
  stacja,
  czas,
  pomiary = []  % List of {Type, Value} tuples
}).

%% API
-export([
  create_monitor/0,
  add_station/3,
  add_value/5,
  remove_value/4,
  get_one_value/4,
  get_station_min/3,
  get_daily_mean/3,
  get_sample_data/0
]).

%%%===================================================================
%%% API Functions
%%%===================================================================

create_monitor() -> [].

add_station(Monitor, Station, Time) ->
  case exists(Monitor, Station, Time) of
    true -> {error, already_exists};
    false ->
      [#pomiar{stacja = Station, czas = Time} | Monitor]
  end.

add_value(Monitor, Station, Time, Type, Value) ->
  case find_record(Monitor, Station, Time) of
    not_found -> {error, station_time_not_found};
    {found, Record} ->
      case lists:keymember(Type, 1, Record#pomiar.pomiary) of
        true -> {error, type_already_exists};
        false ->
          NewMeasurements = [{Type, Value} | Record#pomiar.pomiary],
          NewRecord = Record#pomiar{pomiary = NewMeasurements},
          replace_record(Monitor, Record, NewRecord)
      end
  end.

remove_value(Monitor, Station, Time, Type) ->
  case find_record(Monitor, Station, Time) of
    not_found -> {error, station_time_not_found};
    {found, Record} ->
      case lists:keymember(Type, 1, Record#pomiar.pomiary) of
        false -> {error, type_not_found};
        true ->
          NewMeasurements = lists:keydelete(Type, 1, Record#pomiar.pomiary),
          NewRecord = Record#pomiar{pomiary = NewMeasurements},
          replace_record(Monitor, Record, NewRecord)
      end
  end.

get_one_value(Monitor, Station, Time, Type) ->
  case find_record(Monitor, Station, Time) of
    not_found -> {error, station_time_not_found};
    {found, Record} ->
      case lists:keyfind(Type, 1, Record#pomiar.pomiary) of
        false -> {error, type_not_found};
        {Type, Value} -> {ok, Value}
      end
  end.

get_station_min(Monitor, Station, Type) ->
  Values = [Value ||
    #pomiar{stacja = S, pomiary = Measurements} <- Monitor,
    S =:= Station,
    {T, Value} <- Measurements,
    T =:= Type],
  case Values of
    [] -> {error, no_measurements};
    _ -> {ok, lists:min(Values)}
  end.

get_daily_mean(Monitor, Day, Type) ->
  Values = [Value ||
    #pomiar{czas = Time, pomiary = Measurements} <- Monitor,
    is_same_day(Time, Day),
    {T, Value} <- Measurements,
    T =:= Type],
  case Values of
    [] -> {error, no_measurements};
    _ -> {ok, lists:sum(Values) / length(Values)}
  end.

get_sample_data() ->
  P1 = #pomiar{
    stacja = "Krakow",
    czas = "2025-02-01 14:00",
    pomiary = [{pm10, 1.23}, {pm25, 31.45}, {no2, 0.03}]
  },
  P2 = #pomiar{
    stacja = "Bielsko",
    czas = "2025-03-04 12:00",
    pomiary = [{pm10, 1.01}, {pm25, 21.32}, {no2, 0.06}]
  },
  P3 = #pomiar{
    stacja = "Warszawa",
    czas = "2025-01-14 21:00",
    pomiary = [{pm10, 8.32}, {pm25, 20.0}, {no2, 1.0}]
  },
  [P1, P2, P3].

%%%===================================================================
%%% Internal Helper Functions
%%%===================================================================

exists(Monitor, Station, Time) ->
  lists:any(fun(#pomiar{stacja = S, czas = T}) ->
    S =:= Station andalso T =:= Time
            end, Monitor).

find_record(Monitor, Station, Time) ->
  case lists:filter(fun(#pomiar{stacja = S, czas = T}) ->
    S =:= Station andalso T =:= Time
                    end, Monitor) of
    [] -> not_found;
    [Record] -> {found, Record}
  end.

replace_record(Monitor, OldRecord, NewRecord) ->
  lists:map(fun
              (R) when R =:= OldRecord -> NewRecord;
              (R) -> R
            end, Monitor).

is_same_day(DateTimeStr, Day) ->
  string:substr(DateTimeStr, 1, 10) =:= Day.