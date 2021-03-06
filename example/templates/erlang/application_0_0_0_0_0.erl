%%% File      : application_0_0_0_0_0.erl
%%% Purpose   : Testing the template.
%%% Created   : Sunday, May 23 2021.
%%% Author    : Pierre Rouleau <prouleau001@gmail.com>
%%% ------------------------------------------------------------------------------------------------
%%% Module Description: Testing the template.
%%%
%%% [
%   This text inside the square brackets is normally NOT generated by the template!
%   It is here only to show the value of the user-options used to generate the content.
%   This file was generated by the test function: pel--erlang-all-sk-file-header
%   This file was generated with the following user-options values:
%   - pel-erlang-skel-use-separators        : nil
%   - pel-erlang-skel-use-secondary-separators : nil
%   - pel-erlang-skel-insert-file-timestamp          : nil
%   - pel-erlang-skel-with-license          : nil
%   - pel-erlang-skel-with-edoc         : nil
%    ]
%%% ================================================================================================
-module(application_0_0_0_0_0).

-behaviour(application).

%% Application callbacks
-export([start/2, start_phase/3, stop/1, prep_stop/1,
         config_change/3]).

%%% application_0_0_0_0_0 Application callbacks
%%% ===========================================


%% Private function: Start application
%%
%% Called when an application is started using application:start/[1,2],
%% and application processes should be started.
%% If the application is structured according to the OTP design
%% principles as a supervision tree, this means starting the
%% top supervisor of the tree.

-spec start(StartType :: normal |
                         {takeover, Node :: node()} |
                         {failover, Node :: node()},
            StartArgs :: term()) ->
          {ok, Pid :: pid()} |
          {ok, Pid :: pid(), State :: term()} |
          {error, Reason :: term()}.
start(_StartType, _StartArgs) ->
    case 'TopSupervisor':start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%% Private function: Top supervisor of the tree.
%%
%% Starts an application with included applications, when
%% synchronization is needed between processes in the different
%% applications during startup.

-spec start_phase(Phase :: atom(),
                  StartType :: normal |
                               {takeover, Node :: node()} |
                               {failover, Node :: node()},
                  PhaseArgs :: term()) -> ok | {error, Reason :: term()}.
start_phase(_Phase, _StartType, _PhaseArgs) ->
    ok.

%% Private function: Handle application stop request.
%%
%% Called whenever an application has stopped.
%% It is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.

-spec stop(State :: term()) -> any().
stop(_State) ->
    ok.

%% Private function: Handle preparation of application stop.
%%
%% Called when an application is about to be stopped,
%% before shutting down the processes of the application.

-spec prep_stop(State :: term()) -> NewState :: term().
prep_stop(State) ->
    State.

%% Private function: Handle code replacement configuration.
%%
%% Called by an application after a code replacement,
%% if the configuration parameters have changed.

-spec config_change(Changed :: [{Par :: atom(), Val :: term()}],
                    New :: [{Par :: atom(), Val :: term()}],
                    Removed :: [Par :: atom()]) -> ok.
config_change(_Changed, _New, _Removed) ->
    ok.

%%% application_0_0_0_0_0 Internal functions
%%% ========================================


%%% ------------------------------------------------------------------------------------------------
