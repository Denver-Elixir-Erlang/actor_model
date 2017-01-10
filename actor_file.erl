-module(actor_file).

-import(lists, [append/2]).
-import(math, [sqrt/1]).

-export([
  start/0,
  run/0,
  is_prime/1,
  aggregate/1
]).

start() ->
  Pid = spawn(actor_file, run, []),
  Pid.

process_file(File) ->
  {ok, FD} = file:open(File, read),
  read_lines(FD, []).

read_lines(FD, L) ->
  case file:read_line(FD) of
    {ok, Line} ->
      Num = list_to_integer(
        [X || X <- Line, X /= 10]),
      read_lines(FD, lists:append(L, [Num]));
    eof ->
      distribute(L)
   end.

distribute(Nums) ->
  Agg = spawn(actor_file, aggregate, [[]]),
  io:format("Aggie: ~w~n", [Agg]),
  Pids = [{Num, spawn(F)} || 
    {Num, F} <- [{Num, fun() -> run() end} || 
   Num <- Nums]],
  [send_msg(Num, Pid, Agg) || 
   {Num, Pid} <- Pids].

send_msg(Msg, Pid, Agg) ->
  io:format("~w is running on: ~w~n", [Msg, Pid]),
  Pid ! {self(), {process_line, Msg, Agg}}.

print_results() ->
  io:format("Done~n").

process_line(Line, Pid) ->
  Result = is_prime(Line),
  Pid ! {result, {Line, Result}},
  io:format("Result for ~w : ~w~n", [Line, Result]).

is_prime(N) ->
  SqRt = list_to_integer(float_to_list(math:sqrt(N), [{decimals, 0}])),
  test_primality(2, N, SqRt).

test_primality(I, Num, SqRt) ->
  if
    I =< SqRt -> 
      if 
        Num rem I == 0 ->
          false;
        true ->
          test_primality(I+1, Num, SqRt)
      end;
    true ->
      true
  end.

aggregate(R) ->
  receive
    {result, Results} ->
      S = lists:append(R, [Results]),
      aggregate(S);
    print ->
      io:format("Total Results: ~w~n", [R])
  end.

run() ->
  receive
    {Actor, {process_file, File}} ->
      Actor ! {self(), process_file(File)};
    {Actor, {process_line, Line, Pid}} ->
      Actor ! {self(), process_line(Line, Pid)};
    total ->
      print_results()
  end,
  run().

