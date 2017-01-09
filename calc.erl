-module(calc).
-export([prime_numbers/1]).

% Source: https://wbear.wordpress.com/2011/12/08/prime-numbers-with-erlang/
% Sieve of Eratosthenes algorithm for finding all prime numbers up to N.
% http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes

% Generate list of prime numbers up to N.
prime_numbers(N) when is_number(N) ->
prime_numbers(N, generate(N)).

prime_numbers(Max, [H|T]) when H * H =< Max ->
   [H | prime_numbers(Max, [R || R <- T, (R rem H) > 0])];

prime_numbers(_, T) -> T.

% Generate sequence 2..N
generate(N) -> generate(N, 2).

generate(Max, Max) -> [Max];

generate(Max, X) -> [X | generate(Max, X + 1)].
