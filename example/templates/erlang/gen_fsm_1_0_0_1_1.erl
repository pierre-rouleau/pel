%%% File      : gen_fsm_1_0_0_1_1.erl
%%% Purpose   : Testing the template.
%%% Created   : Sunday, May 23 2021.
%%% @author: Pierre Rouleau <prouleau001@gmail.com>

%%% Copyright (C) 2021  Pierre Rouleau
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.

%%% ------------------------------------------------------------------------------------------------
%%% @doc Testing the template.
%%%      [
%   This text inside the square brackets is normally NOT generated by the template!
%   It is here only to show the value of the user-options used to generate the content.
%   This file was generated by the test function: pel--erlang-all-sk-file-header
%   This file was generated with the following user-options values:
%   - pel-erlang-skel-use-separators        : t
%   - pel-erlang-skel-use-secondary-separators : nil
%   - pel-erlang-skel-insert-file-timestamp          : nil
%   - pel-erlang-skel-with-license          : t
%   - pel-erlang-skel-with-edoc         : t
%    ]
%%% @end
%%% ================================================================================================
-module(gen_fsm_1_0_0_1_1).

-behaviour(gen_fsm).

%% gen_fsm_1_0_0_1_1 API
-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).
-export([state_name/2, state_name/3]). % Example functions: TODO: replace those with your names.

-define(SERVER, ?MODULE).

-record(state, {}).

%%% ================================================================================================
%%% gen_fsm_1_0_0_1_1 API
%%% =====================


%% -------------------------------------------------------------------------------------------------
%% @doc Start and link.
%%      Create a gen_fsm process which calls Module:init/1 to
%%      initialize. To ensure a synchronized start-up procedure, this
%%      function does not return until Module:init/1 has returned.
%% @end

-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Error :: term()}.
start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% ================================================================================================
%%% gen_fsm callbacks
%%% =================


%% -------------------------------------------------------------------------------------------------
%% @private
%% @doc Initialize.
%%      Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%%      gen_fsm:start_link/[3,4], this function is called by the new
%%      process to initialize.
%% @end

-spec init(Args :: term()) -> {ok, StateName :: atom(), State :: term()} |
          {ok, StateName :: atom(), State :: term(), Timeout :: timeout()} |
          ignore |
          {stop, StopReason :: term()}.
init([]) ->
    process_flag(trap_exit, true),
    {ok, state_name, #state{}}.

%% -------------------------------------------------------------------------------------------------
%% @private
%% @doc Handle event at state.
%%      Whenever a gen_fsm receives an event sent using
%%      gen_fsm:send_event/2, the instance of this function with the same
%%      name as the current state name StateName is called to handle
%%      the event. It is also called if a timeout occurs.
%%      TODO: Implement one such function for each state to handle,
%%            replacing 'name' with the state name.
%%            There should be one function like this for each state name.
%% @end

-spec state_name(Event :: term(), State :: term()) ->
          {next_state, NextStateName :: atom(), NextState :: term()} |
          {next_state, NextStateName :: atom(), NextState :: term(), Timeout :: timeout()} |
          {stop, Reason :: term(), NewState :: term()}.
state_name(_Event, State) ->
    {next_state, state_name, State}.

%% -------------------------------------------------------------------------------------------------
%% @private
%% @doc Handle event at state.
%%      Whenever a gen_fsm receives an event sent using
%%      gen_fsm:sync_send_event/[2,3], the instance of this function with
%%      the same name as the current state name StateName is called to
%%      handle the event.
%%      TODO: Implement one such function for each state to handle,
%%            replacing 'name' with the state name.
%%            There should be one function like this for each state name.
%% @end

-spec state_name(Event :: term(), From :: term(), State :: term()) ->
          {next_state, NextStateName :: atom(), NextState :: term()} |
          {next_state, NextStateName :: atom(), NextState :: term(), Timeout :: timeout()} |
          {reply, Reply :: term(), NextStateName :: atom(), NextState :: term()} |
          {reply, Reply :: term(), NextStateName :: atom(), NextState :: term(), Timeout :: timeout()} |
          {stop, Reason :: term(), NewState :: term()} |
          {stop, Reason :: term(), Reply :: term(), NewState :: term()}.
state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

%% -------------------------------------------------------------------------------------------------
%% @private
%% @doc Handle event.
%%      Whenever a gen_fsm receives an event sent using
%%      gen_fsm:send_all_state_event/2, this function is called to handle
%%      the event.
%% @end

-spec handle_event(Event :: term(), StateName :: atom(), State :: term()) ->
          {next_state, NextStateName :: atom(), NextState :: term()} |
          {next_state, NextStateName :: atom(), NextState :: term(), Timeout :: timeout()} |
          {stop, Reason :: term(), NewState ::term()}.
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%% -------------------------------------------------------------------------------------------------
%% @private
%% @doc Handle synchronous event.
%%      Whenever a gen_fsm receives an event sent using
%%      gen_fsm:sync_send_all_state_event/[2,3], this function is called
%%      to handle the event.
%% @end

-spec handle_sync_event(Event :: term(), From :: term(), StateName :: atom(), State :: term()) ->
          {next_state, NextStateName :: atom(), NextState :: term()} |
          {next_state, NextStateName :: atom(), NextState :: term(), Timeout :: timeout()} |
          {reply, Reply :: term(), NextStateName :: atom(), NextState :: term()} |
          {reply, Reply :: term(), NextStateName :: atom(), NextState :: term(), Timeout :: timeout()} |
          {stop, Reason :: term(), NewState :: term()} |
          {stop, Reason :: term(), Reply :: term(), NewState :: term()}.
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%% -------------------------------------------------------------------------------------------------
%% @private
%% @doc Handle info.
%%      This function is called by a gen_fsm when it receives any
%%      message other than a synchronous or asynchronous event
%%      (or a system message).
%% @end

-spec handle_info(Info :: term(), StateName :: atom(), State :: term())->
          {next_state, NextStateName :: atom(), NextState ::term()} |
          {next_state, NextStateName :: atom(), NextState :: term(), Timeout :: timeout()} |
          {stop, Reason :: term(), NewState :: term()}.
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%% -------------------------------------------------------------------------------------------------
%% @private
%% @doc Terminate.
%%      This function is called by a gen_fsm when it is about to
%%      terminate. It should be the opposite of Module:init/1 and do any
%%      necessary cleaning up. When it returns, the gen_fsm terminates with
%%      Reason. The return value is ignored.
%% @end

-spec terminate(Reason :: term(), StateName :: atom(), State :: term()) -> OK.
terminate(_Reason, _StateName, _State) ->
    ok.

%% -------------------------------------------------------------------------------------------------
%% @private
%% @doc Handle code change.
%%      Convert process state when code is changed.
%% @end

-spec code_change(OldVsn :: term(), StateName :: atom(), State :: term(), Extra :: term()) ->
          {ok, StateName :: atom(), NewState :: term()}.
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%% ================================================================================================
%%% gen_fsm_1_0_0_1_1 Internal functions
%%% ====================================


%%% ------------------------------------------------------------------------------------------------