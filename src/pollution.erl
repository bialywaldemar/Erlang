%%%-------------------------------------------------------------------
%%% @author mateu
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. cze 2025 11:51
%%%-------------------------------------------------------------------
-module(pollution).
-author("mateu").

-record(pomiar, {
  stacja,
  czas,
  wartosci
}).

%% API
-export([f/0, f/1, power/2, get_sample_data/0, number_of_readings/2, calculate_max/2, calculate_mean/2]).

f() -> 42.

f(abc) -> io:format("helloworld");
f(N) ->
  Z = f() + N,
  Z.

power(A, B) ->
  Z = math:pow(A,B),
  Z.

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

number_of_readings(Readings, Date) ->
  lists:foldl(fun(#pomiar{czas = Czas}, Acc) ->
    case string:find(Czas, Date) of
      Czas -> Acc + 1;  % Data na początku stringa
      _ -> Acc
    end
  end, 0, Readings).

calculate_max(Readings, Type) ->
  Values = lists:filtermap(fun(#pomiar{wartosci = W}) ->
    case length(W) >= Type of
      true -> {true, lists:nth(Type, W)};
      false -> false
    end
  end, Readings),
  case Values of
    [] -> 0.0;
    _ -> lists:max(Values)
  end.

calculate_mean(Readings, Type) ->
  {Sum, Count} = lists:foldl(fun(#pomiar{wartosci = W}, {S, C}) ->
    case length(W) >= Type of
      true -> {S + lists:nth(Type, W), C + 1};
      false -> {S, C}
    end
  end, {0.0, 0}, Readings),
  case Count of
    0 -> 0.0;
    _ -> Sum / Count
  end.

