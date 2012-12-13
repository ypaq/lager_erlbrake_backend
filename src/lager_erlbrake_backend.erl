%% @doc Airbrake lager backend.
%%      
%% Notifies Airbrake of error (or more critical) log messages from lager.
%%
%% More info on Lager see: https://github.com/basho/lager/
%% More info on Airbrake see: http://airbrake.io/
%%
-module(lager_erlbrake_backend).

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
  application:set_env(erlbrake, error_logger, false), % not needed with lager
  case application:start(erlbrake, permanent) of
    {error, {already_started, erlbrake}} -> ok;
    ok -> ok
  end,
  {ok, #state{notify_level = lager_util:level_to_num(NotifyLevel)}}.

%% @private
handle_call(get_loglevel, #state{notify_level=Level} = State) ->
  {ok, Level, State};
handle_call({set_loglevel, Level}, State) ->
  {ok, ok, State#state{notify_level=lager_util:level_to_num(Level)}};
handle_call(_Request, State) ->
  {ok, State}.

%% @private
handle_event({log, LogMsg}, #state{notify_level = NotifyLevel} = State)  ->
  case lager_util:is_loggable(LogMsg, NotifyLevel, airbrake) of
    true -> notify_airbrake(LogMsg);
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

%% local functions

notify_airbrake(LogMsg) ->
  Severity = lager_msg:severity(LogMsg),
  Message = lager_msg:message(LogMsg),
  Metadata = lager_msg:metadata(LogMsg),
  Line = proplists:get_value(line, Metadata),
  Module = proplists:get_value(module, Metadata),
  Function = proplists:get_value(function, Metadata),
  Pid = proplists:get_value(pid, Metadata),
  Node = proplists:get_value(node, Metadata),
  Application = get_application(Pid, Module),
  airbrake:notify([{reason, Severity}, 
                   {message, Message},
                   {module, Module},
                   {function, Function},
                   {line, Line},
                   {node, Node},
                   {pid, Pid},
                   {application, Application}
                  ]).

get_application(Pid, Module) ->
  case application:get_application(Pid) of
    undefined -> 
      case application:get_application(Module) of
        {ok, App} -> App;
        _ -> undefined
      end;
    {ok, App} -> App
  end.

