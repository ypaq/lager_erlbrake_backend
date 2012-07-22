%% @doc Airbrake lager backend.
%%      
%% Notifies Airbrake of error (or more critical) log messages from lager.
%%
%% More info on Lager see: https://github.com/basho/lager/
%% More info on Airbrake see: http://airbrake.io/
%%
-module(lager_erlbrake_backend).

-behaviour(gen_event).

-include_lib("lager/include/lager.hrl").

-export([init/1, 
         handle_call/2,
         handle_event/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-define(AIRBRAKE_LEVELS, [error, critical, alert, emergency]).

-record(state, {}).

%% @private
-spec init([{atom(), string()}]) -> {ok, #state{}}.
init([{environment, Environment}, {api_key, ApiKey}]) ->
  application:set_env(erlbrake, api_key, ApiKey),
  application:set_env(erlbrake, environment, ApiKey),
  ok = application:start(erlbrake),
  {ok, #state{}}.

%% @private
handle_call(_Request, State) ->
  {ok, State}.

%% @private
handle_event(#lager_log_message{severity_as_int=L} = Log, #state{} = State)  ->
  %% notify every error (or more critical) log messages for now
  case lists:member(lager_util:num_to_level(L), ?AIRBRAKE_LEVELS) of
    true  -> notify_airbrake(Log);
    false -> ok
  end,
  {ok, State};
handle_event(_Event, State) ->
  {ok, State}.

%% @private
handle_info(_Info, State) ->
  {ok, State}.

%% @private
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% @private
terminate(_Reason, _State) ->
  ok.

notify_airbrake(#lager_log_message{message = Message,
                                   timestamp = {Date, Time},
                                   metadata = Metadata,
                                   severity_as_int = L} = Log) ->
  Severity = lager_util:num_to_level(L),
  Pid = get_metadata(pid, Metadata),
  Line = get_metadata(line, Metadata),
  Module = get_metadata(module, Metadata),
  %% TODO: improve erlbrake to handle these parameters
  _Function = get_metadata(function, Metadata),
  _Node = get_metadata(node, Metadata),
  airbrake:notify(ignored, Severity, Message, Module, Line).


get_metadata(Key, Metadata) ->
  get_metadata(Key, Metadata, undefined).

get_metadata(Key, Metadata, Default) ->
  case lists:keyfind(Key, 1, Metadata) of
    false ->
      Default;
    {Key, Value} ->
      Value
  end.


