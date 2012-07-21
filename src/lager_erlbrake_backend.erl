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
init({}) ->
  application:start(erlbrake, permanent),

  {ok, #state{}}.

%% @private
handle_call(_Request, State) ->
  {ok, State}.

%% @private
handle_event(#lager_log_message{severity_as_int=L} = Log, #state{} = State) 
    when L <= ?ERROR ->
  %% notify every error (or more critical) log messages for now
  notify_airbrake(Log);
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
                                   timestamp = Timestamp,
                                   metadata = Metadata,
                                   severity_as_int = L} = Log) ->
  Severity = ?NUM2LEVEL(L),
  Date = get_date,
  Time = get_time,
  Pid = pid, 
  Line = line, 
  Module = module, 
  Function = function, 
  Node = node
  airbrake:notify(ignored, Level, Message, unknown, 0).



