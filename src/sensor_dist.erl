%%%-------------------------------------------------------------------
%%% @author mateu
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. cze 2025 19:15
%%%-------------------------------------------------------------------
-module(sensor_dist).
-author("mateu").

%% API
-export([get_rand_locations/1, dist/2, find_for_person/2, find_closest/2, find_for_person/3, find_closest_parallel/2, fac/1]).


dist({X1, Y1}, {X2, Y2}) ->
  math:sqrt(math:pow(X1 - X2, 2) + math:pow(Y1 - Y2, 2)).

get_rand_locations(N) ->
  [{rand:uniform(10000), rand:uniform(10000)} || _ <- lists:seq(1, N)].

find_for_person(PersonLocation, SensorsLocations) -> % wyszuka i zwróci {dystans, {pozycjaOsoby, pozycjaCzujnika}}
  Dists = [{dist(PersonLocation, Sensor), {PersonLocation, Sensor}} || Sensor <- SensorsLocations],
  lists:min(Dists).

find_closest(PeopleLocations, SensorsLocations) -> %  użyje poprzedniej funkcji i zwróci {dystans, {pozycjaOsoby, pozycjaCzujnika}}
  PerPersonRes = [find_for_person(Person, SensorsLocations) || Person <- PeopleLocations],
  lists:min(PerPersonRes).

find_for_person(PersonLocation, SensorsLocations, ParentPID) -> % po obliczeniu wyniku odsyła go do ParentPID
  Result = find_for_person(PersonLocation, SensorsLocations),
  ParentPID ! {result, self(), Result},
  ok.

find_closest_parallel(PeopleLocations, SensorsLocations) ->
  Processes = [spawn(?MODULE, find_for_person, [PersonLoc, SensorsLocations, self()]) || PersonLoc <- PeopleLocations], % uruchomi wyszukiwanie w osobnym procesie dla każdej osoby oddzielnie,
  Results = collect_results(length(Processes)), % odbierz komunikaty, zbierz wszystkie wyniki od procesów w liści
  lists:min(Results). % wyszuka wynik funkcją lists:min.

collect_results(0) -> [];
collect_results(N) ->
  receive
    {result, _PID, Result} ->
      [Result | collect_results(N-1)]
  after 5000 -> % timeout 5 sekund dla bezpieczeństwa
    exit(timeout)
  end.

fac(0) -> 1;
fac(N) when N > 0 -> N * fac(N - 1).
