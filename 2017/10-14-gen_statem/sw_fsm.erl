-module(sw_fsm).
-behaviour(gen_fsm).

%% for gen_fsm, OTP20
-compile([nowarn_deprecated_function]).

%% gen_fsm
-export([start_link/0, init/1, waiting/2, measuring/2, paused/2,
  handle_sync_event/4, handle_event/3, handle_info/3,
  terminate/3, code_change/4]).

%% interface
-export([push_start/0, push_stop/0, push_reset/0, show/0,
  exit/0]).

push_start() ->
  gen_fsm:send_event(?MODULE, start).

push_stop() ->
  gen_fsm:send_event(?MODULE, stop).

push_reset() ->
  gen_fsm:send_event(?MODULE, reset).

show() ->
  gen_fsm:sync_send_all_state_event(?MODULE, show).

exit() ->
  exit(whereis(?MODULE)).

start_link() ->
  gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  {ok, waiting, #{display => 0}}.

handle_event(Event, StateName, Data) ->
  {stop, {shutdown, {unexpected, Event, StateName}}, Data}.

handle_sync_event(show, _From, StateName, Data) ->
  Number = maps:get(display, Data, undefined),
  Reply = [{state, StateName}, {display, Number}],
  {reply, Reply, StateName, Data, 1000};
handle_sync_event(_Event, _From, StateName, Data) ->
  {next_state, StateName, Data}.

handle_info(Info, StateName, Data) ->
  {stop, {shutdown, {unexpected, Info, StateName}}, StateName, Data}.

terminate(Reason, _State, _Data) ->
  io:format("terminate Reason:~p~n", [Reason]),
  ok.

code_change(_Vsn, State, Data, _Extra) ->
  {ok, State, Data}.

waiting(start, Data) ->
  {next_state, measuring, Data, 1000};
waiting(_Event, Data) ->
  {next_state, waiting, Data}.

measuring(timeout, Data) ->
  Number = maps:get(display, Data, 0),
  {next_state, measuring, Data#{display => Number + 1}, 1000};
measuring(stop, Data) ->
  {next_state, paused, Data};
measuring(_Event, Data) ->
  {next_state, measuring, Data}.

paused(start, Data) ->
  {next_state, measuring, Data};
paused(reset, Data) ->
  {next_state, waiting, Data#{display => 0}};
paused(_Event, Data) ->
  {next_state, paused, Data}.
