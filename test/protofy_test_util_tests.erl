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

-module(protofy_test_util_tests).

-include_lib("eunit/include/eunit.hrl").

-export([]).

%% Test unmock/1
%% ====================================================================
unmock_1_test_() ->
  R0 = protofy_test_util:unmock(base64),
  ok = meck:new(base64, [unstick]),
  ok = meck:expect(base64, encode, fun(_) -> <<"MOCKED">> end),
  R1 = base64:encode("Test"),
  protofy_test_util:unmock(base64),
  R2 = base64:encode("Test"),
  [
   {"Unmocked before", ?_assertEqual(ok, R0)},
   {"Mocked", ?_assertEqual(<<"MOCKED">>, R1)},
   {"Unmocked", ?_assertEqual(<<"VGVzdA==">>, R2)}
  ].


%% Test rcv/1
%% ====================================================================
rcv_1_test_() ->
  R1 = protofy_test_util:rcv(10),
  self() ! test,
  R2 = protofy_test_util:rcv(10),
  R3 = protofy_test_util:rcv(10),
  [
   {"Before sending", ?_assertEqual({error, timeout}, R1)},
   {"After sending", ?_assertEqual(test, R2)},
   {"After receiving", ?_assertEqual({error, timeout}, R3)}
  ].

%% Test wait_for_stop/2
%% ====================================================================
wait_for_stop_2_unnamed_ok_test() ->
  Pid = proc_lib:spawn(fun() -> timer:sleep(10) end),
  ?assertEqual(ok, protofy_test_util:wait_for_stop(Pid, 20)).

wait_for_stop_2_unnamed_timeout_test() ->
  Pid = proc_lib:spawn(fun() -> timer:sleep(20) end),
  ?assertEqual({error, timeout}, protofy_test_util:wait_for_stop(Pid, 5)).

wait_for_stop_2_named_ok_test() ->
  Pid = proc_lib:spawn(fun() -> timer:sleep(10) end),
  Name = protofy_test_util_proc_1,
  register(Name, Pid),
  ?assertEqual(ok, protofy_test_util:wait_for_stop(Name, 20)).

wait_for_stop_2_named_timeout_test() ->
  Pid = proc_lib:spawn(fun() -> timer:sleep(100) end),
  Name = protofy_test_util_proc_2,
  register(Name, Pid),
  ?assertEqual({error, timeout}, protofy_test_util:wait_for_stop(Name, 5)).

wait_for_stop_2_named_not_started_test() ->
  Name = protofy_test_util_proc_3,
  ?assertEqual(ok, protofy_test_util:wait_for_stop(Name, 5)).


%% Test fetch_ets/3
%% ====================================================================
fetch_ets_3_test_() ->
  {setup,
   fun() ->
       Tab = ets:new(protofy_test_util_test_fetch_ets, [public, named_table]),
       ets:insert(Tab, {test_2, test_value}),
       Tab
   end,
   fun(Tab) ->
       ets:delete(Tab)
   end,
   fun(Tab) ->
       [
        {"timeout", ?_assertEqual({error, timeout}, protofy_test_util:fetch_ets(Tab, test_1, 5))},
        {"found", ?_assertEqual({ok, {test_2, test_value}}, protofy_test_util:fetch_ets(Tab, test_2, 10))}
       ]
   end}.


%% Test expect_ets/4
%% ====================================================================
expect_ets_4_test_() ->
  {setup,
   fun() ->
       Tab = ets:new(protofy_test_util_test_ets, [public, named_table]),
       Tab
   end,
   fun(Tab) ->
       ets:delete(Tab)
   end,
   fun(Tab) ->
       [
        {"timeout", ?_assertEqual({error, timeout}, protofy_test_util:expect_ets(Tab, test_1, test_2, 5))},
        {"ok with fun", fun() -> test_expect_ets_4_match_fun_ok(Tab) end},
        {"unexpected with fun", fun() -> test_expect_ets_4_match_fun_unexpected(Tab) end},
        {"ok with term", fun() -> test_expect_ets_4_match_term_ok(Tab) end},
        {"unexpected with term", fun() -> test_expect_ets_4_match_term_unexpected(Tab) end}
       ]
   end}.

test_expect_ets_4_match_fun_ok(Tab) ->
  ets:insert(Tab, {test_3, test_4}),
  Fun = fun(test_4) -> true;
           (_)      -> false end,
  ?assertEqual(ok, protofy_test_util:expect_ets(Tab, test_3, Fun, 10)).

test_expect_ets_4_match_fun_unexpected(Tab) ->
  ets:insert(Tab, {test_5, test_6}),
  Fun = fun(test_4) -> true;
           (_)      -> false end,
  ?assertEqual({unexpected, {test_5, test_6}}, protofy_test_util:expect_ets(Tab, test_5, Fun, 10)).

test_expect_ets_4_match_term_ok(Tab) ->
  ets:insert(Tab, {test_7, test_8}),
  ?assertEqual(ok, protofy_test_util:expect_ets(Tab, test_7, test_8, 10)).

test_expect_ets_4_match_term_unexpected(Tab) ->
  ets:insert(Tab, {test_9, test_10}),
  ?assertEqual({unexpected, {test_9, test_10}}, protofy_test_util:expect_ets(Tab, test_9, test_8, 10)).

%% ====================================================================
%% Internal functions
%% ====================================================================


