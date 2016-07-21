%% ====================================================================
%%
%% Copyright (c) Protofy GmbH & Co. KG, Kaiser-Wilhelm-Straße 85, 20355 Hamburg/Germany and individual contributors.
%% All rights reserved.
%% 
%% Redistribution and use in source and binary forms, with or without modification,
%% are permitted provided that the following conditions are met:
%% 
%%     1. Redistributions of source code must retain the above copyright notice,
%%        this list of conditions and the following disclaimer.
%% 
%%     2. Redistributions in binary form must reproduce the above copyright
%%        notice, this list of conditions and the following disclaimer in the
%%        documentation and/or other materials provided with the distribution.
%% 
%%     3. Neither the name of Protofy GmbH & Co. KG nor the names of its contributors may be used
%%        to endorse or promote products derived from this software without
%%        specific prior written permission.
%% 
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
%% ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%
%% ====================================================================
%%
%% @author Bjoern Kortuemm (@uuid0) <bjoern@protofy.com>
%% @doc Some helper functions to be used in test contexts (eunit, ct, ...).


-module(protofy_test_util).

-type key() :: term().

%% ====================================================================
%% API functions
%% ====================================================================
-export([
  unmock/1,
  rcv/1,
  wait_for_stop/2,
  fetch_ets/3,
  expect_ets/4
]).


%% unmock/1
%% ====================================================================
%% @doc make sure given Mod is not mocked
-spec unmock(module()) -> ok.
%% ====================================================================
unmock(Mod) ->
  try
    meck:unload(Mod),
    code:ensure_loaded(Mod)
  catch
    error:{not_mocked,Mod} -> ok
  end.


%% rcv/1
%% ====================================================================
%% @doc Receive any message with Timeout
-spec rcv(Timeout :: non_neg_integer()) -> {error, timeout} | term().
%% ====================================================================
rcv(Timeout) ->
  receive
    X -> X
  after
      Timeout -> {error, timeout}
  end.


%% wait_for_stop/2
%% ====================================================================
%% @doc Wait for a process to be dead.
%%
%% This function will do a sleep-spin-wait Tries times. It will sleep
%% for 1 ms between tries, so you could interpret it as a rough timeout
%% in ms.
-spec wait_for_stop(pid() | Name, Tries) -> ok | {error, timeout} when
  Name :: atom(),
  Tries :: non_neg_integer().
%% ====================================================================
wait_for_stop(_, 0) ->
  {error, timeout};
wait_for_stop(Pid, Tries) when is_pid(Pid) ->
  case is_process_alive(Pid) of
    true -> timer:sleep(1), wait_for_stop(Pid, Tries-1);
    false -> ok
  end;
wait_for_stop(Name, Tries) when is_atom(Name) ->
  case whereis(Name) of
    undefined -> ok;
    Pid when is_pid(Pid) -> wait_for_stop(Pid, Tries)
  end.


%% fetch_ets/3
%% ====================================================================
%% @doc Try to fetch K from ets Tab.
%%
%% This function will do a sleep-spin-wait Tries times. It will sleep
%% for 1 ms between tries, so you could interpret it as a rough timeout
%% in ms.
-spec fetch_ets(Tab, key(), Tries) -> {ok, term()} | {error, timeout} when
  Tab :: ets:tid(),
  Tries :: non_neg_integer().
%% ====================================================================
fetch_ets(_, _, 0) ->
  {error, timeout};
fetch_ets(Tab, K, Tries) ->
  case ets:lookup(Tab, K) of
    [] ->
      timer:sleep(1),
      fetch_ets(Tab, K, Tries-1);
    [X] ->
      {ok, X}
  end.


%% expect_ets/4
%% ====================================================================
%% @doc Expect a certain term to be written to the given ets Tab.
%%
%% This function will do a sleep-spin-wait Tries times. It will sleep
%% for 1 ms between tries, so you could interpret it as an inaccurate
%% timeout in ms.
%% 
%% You can either specify V to be a predicate function or a term that
%% has to match the written value.
-spec expect_ets(Tab, key(), Value, Tries) -> ok | {error, timeout} | {unexpected, term()} when
  Tab :: ets:tid(),
  Value :: term(),
  Tries :: non_neg_integer().
%% ====================================================================
expect_ets(Tab, K, V, Tries) ->
  case fetch_ets(Tab, K, Tries) of
    {error, timeout} ->
      {error, timeout};
    {ok, {K, X}} when is_function(V) ->
      case V(X) of
        true -> ok;
        false -> {unexpected, {K, X}}
      end;
    {ok, {K, V}} ->
      ok;
    {ok, X} ->
      {unexpected, X}
  end.

