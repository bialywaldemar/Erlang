-module(pollution_gen_server).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").
%%-compile({parse_transform, lager_transform}). %% jeśli używasz lagera do logowania

-record(pomiar, {
  stacja,
  czas,
  pomiary = []  %% [{Type, Value}]
}).

%% API
-export([
  start_link/0,
  crash/0,
  add_station/2,
  add_value/4,
  get_one_value/3
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

%%%===============================
%%% PUBLIC API
%%%===============================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

crash() ->
  gen_server:call(?MODULE, crash).

add_station(Station, Time) ->
  gen_server:call(?MODULE, {add_station, Station, Time}).

add_value(Station, Time, Type, Value) ->
  gen_server:call(?MODULE, {add_value, Station, Time, Type, Value}).

get_one_value(Station, Time, Type) ->
  gen_server:call(?MODULE, {get_one_value, Station, Time, Type}).

%%%===============================
%%% CALLBACKS
%%%===============================

init([]) ->
  %% Stan to lista rekordów #pomiar{}
  {ok, []}.

handle_call(crash, _From, State) ->
  1 div 0,  %% wywołanie awarii
  {reply, ok, State};

handle_call({add_station, Station, Time}, _From, State) ->
  case exists(State, Station, Time) of
    true ->
      {reply, {error, already_exists}, State};
    false ->
      NewState = [#pomiar{stacja=Station, czas=Time} | State],
      {reply, ok, NewState}
  end;

handle_call({add_value, Station, Time, Type, Value}, _From, State) ->
  case find_record(State, Station, Time) of
    not_found ->
      {reply, {error, station_time_not_found}, State};
    {found, Record} ->
      case lists:keymember(Type, 1, Record#pomiar.pomiary) of
        true ->
          {reply, {error, type_already_exists}, State};
        false ->
          NewMeasurements = [{Type, Value} | Record#pomiar.pomiary],
          NewRecord = Record#pomiar{pomiary = NewMeasurements},
          NewState = replace_record(State, Record, NewRecord),
          {reply, ok, NewState}
      end
  end;

handle_call({get_one_value, Station, Time, Type}, _From, State) ->
  case find_record(State, Station, Time) of
    not_found ->
      {reply, {error, station_time_not_found}, State};
    {found, Record} ->
      case lists:keyfind(Type, 1, Record#pomiar.pomiary) of
        false -> {reply, {error, type_not_found}, State};
        {Type, Value} -> {reply, {ok, Value}, State}
      end
  end;

handle_call(_Request, _From, State) ->
  {reply, {error, not_implemented}, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===============================
%%% INTERNAL HELPERS
%%%===============================

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
