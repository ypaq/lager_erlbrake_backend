%% @doc Airbrake lager backend
%%      
%%      Notifies Airbrake of error (or more critical) log messages from lager.
%%
%%      More info on Airbrake see: http://airbrake.io/
%%      More info on Lager see: https://github.com/basho/lager/
%%
%%      Created: Jul 17 2012
%%
-module(lager_erlbrake_backend).
-author('Tilman Holschuh <tilman.holschuh@gmail.com>').

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
-spec init({}) -> {ok, #state{}}.
init({}) ->
  %% TODO: make sure erlbrake is running
  {ok, #state{}}.

%% @private
handle_call(_Request, State) ->
  {ok, State}.

%% @private
handle_event(Log = {log, Dest, Level, {Date, Time}, Message}, State) 
    when Level > ?ERROR -> 
  case lists:member(lager_erlbrake_backend, Dest) of
    true -> notify_airbrake(Log);
    false -> {ok, State}
  end;
handle_event(Log = {log, Level, {Date, Time}, Message}, State) 
    when Level > ?ERROR -> 
  notify_airbrake(Log),
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

notify_airbrake({log, _, Level, {Date, Time}, Message}) ->
  notify_airbrake({log, Level, {Date, Time}, Message});
notify_airbrake({log,  Level, {Date, Time}, Message}) ->
  %% TODO: Lots of information contained in Message but requires string parsing.
  %%       Lager should provide structured way to access log data.
  airbrake:notify(ignored, Level, Message, unknown, 0).



