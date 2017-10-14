-module(sw_statem).
-behaviour(gen_statem).

%% gen_statem
-export([start_link/0, init/1, callback_mode/0, waiting/3, measuring/3, paused/3]).

%% interface
-export([push_start/0, push_stop/0, push_reset/0, show/0,
  exit/0]).

push_start() ->
  gen_statem:cast(?MODULE, start).

push_stop() ->
  gen_statem:cast(?MODULE, stop).

push_reset() ->
  gen_statem:cast(?MODULE, reset).

show() ->
  gen_statem:call(?MODULE, show).

exit() ->
  exit(whereis(?MODULE)).

start_link() ->
  gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  {ok, waiting, #{display => 0}}.

handle_sync_event(show, From, StateName, Data) ->
  Number = maps:get(display, Data, undefined),
  Reply = [{state, StateName}, {display, Number}],
  {keep_state, Data, [{reply, From, Reply}, {timeout, 1000, []}]};
handle_sync_event(_Event, _From, StateName, Data) ->
  {next_state, StateName, Data}.

waiting(cast, start, Data) ->
  {next_state, measuring, Data, 1000};
waiting({call, From}, show, Data) ->
  handle_sync_event(show, From, waiting, Data);
waiting(_EventType, _EventContent, Data) ->
  {next_state, waiting, Data}.

measuring(timeout, _EventContent, Data) ->
  Number = maps:get(display, Data, 0),
  {next_state, measuring, Data#{display => Number + 1}, 1000};
measuring(cast, stop, Data) ->
  {next_state, paused, Data};
measuring({call, From}, show, Data) ->
  handle_sync_event(show, From, measuring, Data);
measuring(_EventType, _EventContent, Data) ->
  {next_state, measuring, Data}.

paused(cast, start, Data) ->
  {next_state, measuring, Data};
paused(cast, reset, Data) ->
  {next_state, waiting, Data#{display => 0}};
paused({call, From}, show, Data) ->
  handle_sync_event(show, From, paused, Data);
paused(_EventType, _EventContent, Data) ->
  {next_state, paused, Data}.

callback_mode() ->
  state_functions.
