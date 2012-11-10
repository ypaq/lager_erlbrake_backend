%% @doc Airbrake lager backend.
%%      
%% Notifies Airbrake of error (or more critical) log messages from lager.
%%
%% More info on Lager see: https://github.com/basho/lager/
%% More info on Airbrake see: http://airbrake.io/
%%
-module(lager_erlbrake_backend).


-include_lib("lager/include/lager.hrl").

-behaviour(gen_event).
-export([init/1, 
         handle_call/2,
         handle_event/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(state, {notify_level}).

%% @private
-spec init([{atom(), term()}]) -> {ok, #state{}}.
init(Args) when is_list(Args) ->
  Environment = proplists:get_value(environment, Args),
  ApiKey = proplists:get_value(api_key, Args),
  NotifyLevel = proplists:get_value(notify_level, Args, error),
  application:set_env(erlbrake, api_key, ApiKey),
  application:set_env(erlbrake, environment, Environment),
  case application:start(erlbrake, permanent) of
    {error, {already_started, erlbrake}} -> ok;
    ok -> ok
  end,
  {ok, #state{notify_level = lager_util:level_to_num(NotifyLevel)}}.

%% @private
handle_call(_Request, State) ->
  {ok, State}.

%% @private
handle_event(LogMsg, #state{notify_level = NotifyLevel} = State)  ->
  case lager_msg:severity_as_int(LogMsg) of
    Severity when Severity =< NotifyLevel, Severity >= 0 -> 
      notify_airbrake(LogMsg);
    _ -> ok
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

notify_airbrake(LogMsg) ->
  Severity = lager_msg:severity(LogMsg),
  Message = lager_msg:message(LogMsg),
  Line = get_metadata(line, LogMsg),
  Module = get_metadata(module, LogMsg),
  Function = get_metadata(function, LogMsg),
  Node = get_metadata(node, LogMsg),
  Pid = get_metadata(pid, LogMsg),
  Application = application:get_application(Pid),
  airbrake:notify([{reason, Severity}, 
                   {message, Message},
                   {module, Module},
                   {function, Function},
                   {line, Line},
                   {node, Node},
                   {pid, Pid},
                   {application, Application}
                  ]).

get_metadata(Key, LogMsg) ->
  Metadata = lager_msg:metadata(LogMsg),
  get_metadata(Key, Metadata, undefined).

get_metadata(Key, Metadata, Default) ->
  case lists:keyfind(Key, 1, Metadata) of
    false -> Default;
    {Key, Value} -> Value
  end.


