%%% File      : gen_event_1_0_1_1_2.erl
%%% Purpose   : Testing the template.
%%% Created   : Sunday, May 23 2021.
%%% Author    : Pierre Rouleau <prouleau001@gmail.com>
%%% Time-stamp: <2021-05-23 02:43:21, updated by Pierre Rouleau>

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
%%% Module Description: Testing the template.
%%%
%%% [
%   This text inside the square brackets is normally NOT generated by the template!
%   It is here only to show the value of the user-options used to generate the content.
%   This file was generated by the test function: pel--erlang-all-sk-file-header
%   This file was generated with the following user-options values:
%   - pel-erlang-skel-use-separators        : t
%   - pel-erlang-skel-use-secondary-separators : nil
%   - pel-erlang-skel-insert-file-timestamp          : t
%   - pel-erlang-skel-with-license          : t
%   - pel-erlang-skel-with-edoc         : in-function-only
%    ]
%%% ================================================================================================
-module(gen_event_1_0_1_1_2).

-behaviour(gen_event).

%% gen_event_1_0_1_1_2 API
-export([start_link/0, add_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%% ================================================================================================
%%% gen_event_1_0_1_1_2 API
%%% =======================


%% -------------------------------------------------------------------------------------------------
%% @doc Create an event manager.

-spec start_link() -> {ok, Pid :: pid()} |
          {error, Error :: {already_started, pid()} | term()}.
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%% -------------------------------------------------------------------------------------------------
%% @doc Add an event handler.

-spec add_handler() -> ok | {'EXIT', Reason :: term()} | term().
add_handler() ->
    gen_event:add_handler(?SERVER, ?MODULE, []).

%%% ================================================================================================
%%% gen_event callbacks
%%% ===================


%% -------------------------------------------------------------------------------------------------
%% @private
%% @doc Initialize the event handler.
%%      Whenever a new event handler is added to an event manager,
%%      this function is called to initialize the event handler.
%% @end

-spec init(Args :: term() | {Args :: term(), Term :: term()}) ->
          {ok, State :: term()} |
          {ok, State :: term(), hibernate} |
          {error, Reason :: term()}.
init([]) ->
    {ok, #state{}}.

%% -------------------------------------------------------------------------------------------------
%% @private
%% @doc Handle event.
%%      Called whenever an event manager receives an event sent using
%%      gen_event:notify/2 or gen_event:sync_notify/2, it is
%%      called for each installed event handler to handle the event.
%% @end

-spec handle_event(Event :: term(), State :: term()) ->
          {ok, NewState :: term()} |
          {ok, NewState :: term(), hibernate} |
          remove_handler |
          {swap_handler, Args1 :: term(), NewState :: term(),
           Handler2 :: atom() | {atom(), term()} , Args2 :: term()}.
handle_event(_Event, State) ->
    {ok, State}.

%% -------------------------------------------------------------------------------------------------
%% @private
%% @doc Handle request.
%%      Called when an event manager receives a request sent using
%%      gen_event:call/3,4. It's called  for the specified
%%      event handler to handle the request.
%% @end

-spec handle_call(Request :: term(), State :: term()) ->
          {ok, Reply :: term(), NewState :: term()} |
          {ok, Reply :: term(), NewState :: term(), hibernate} |
          {remove_handler, Reply :: term()} |
          {swap_handler, Reply :: term(), Args1 :: term(), NewState :: term(),
           Handler2 :: atom() | {atom(), term()}, Args2 :: term()}.
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%% -------------------------------------------------------------------------------------------------
%% @private
%% @doc Handle info.
%%      Called for each installed event handler when
%%      an event manager receives any other message than an event or a
%%      synchronous request (or a system message).
%% @end

-spec handle_info(Info :: term(), State :: term()) ->
          {ok, NewState :: term()} |
          {ok, NewState :: term(), hibernate} |
          remove_handler |
          {swap_handler, Args1 :: term(), NewState :: term(),
           Handler2 :: atom() | {atom(), term()}, Args2 :: term()}.
handle_info(_Info, State) ->
    {ok, State}.

%% -------------------------------------------------------------------------------------------------
%% @private
%% @doc Terminate.
%%      Called when an event handler is deleted from an event manager.
%%      It should be the opposite of Module:init/1 and
%%      do any necessary cleaning up.
%% @end

-spec terminate(Arg :: {stop, Reason :: term()} |
                       stop |
                       remove_handler |
                       {error, {'EXIT', Reason :: term()}} |
                       {error, Term :: term()} |
                       term(),
                State :: term()) -> any().
terminate(_Arg, _State) ->
    ok.

%% -------------------------------------------------------------------------------------------------
%% @private
%% @doc Convert process state when code is changed.

-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -------------------------------------------------------------------------------------------------
%% @private
%% @doc Handle status change request.
%%      Called for changing the form and appearance
%%      of gen_event status when it is returned from sys:get_status/1,2
%%      or when it appears in termination error logs.
%% @end

-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%% ================================================================================================
%%% gen_event_1_0_1_1_2 Internal functions
%%% ======================================


%%% ------------------------------------------------------------------------------------------------