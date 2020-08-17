;;; pel-erlang-skels.el --- Erlang specific tempo skeletons

;; Copyright (C) 2020  Pierre Rouleau

;; Author: Pierre Rouleau <prouleau001@gmail.com>

;; This file is part of the PEL package
;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; -----------------------------------------------------------------------------
;;; Commentary:
;;
;; This file contains Erlang tempo skeletons, new ones and updates to the ones
;; available in erlang-skels.el which are not flexible enough for the
;; formatting.  I would like to make them even more flexible, being able to
;; adjust them dynamically against the version of Erlang, but I have to build a
;; better infrastructure first.  I started with some formatting and will add
;; more later.  For now the code goes and updates the standard Erlang
;; templates.


(require 'pel--base)          ; use: pel-current-buffer-filename
(require 'pel--options)       ; use: pel-erlang-skel-use-separators
;;                            ;      pel-erlang-skel-use-secondary-separators
(require 'pel--macros)
(require 'pel-list)           ; use: pel-insert-list-in-list, pel-join
(require 'pel-tempo)          ; use: pel-tempo-mode,
;;                            ;      pel-tempo-install-pel-skel
(require 'pel-skels)


;; -----------------------------------------------------------------------------
;; Functions used in Erlang tempo skeletons

;; --
;; Line separators

;;; Code:

(defun pel-erlang-skel-separator (&optional percent char)
  "Return a comment separator line of `fill-column' length.
The comment uses PERCENT number of '%'.
The line is made with '-' unless another CHAR is specified.
Note: the smallest allowed `fill-column' value is 70."
  (let ((percent (or percent 3))
        (char    (or char ?-)))
    (concat (make-string percent ?%)
            " "
            (make-string (- (max fill-column 70) percent 1) char)
            "\n")))

(defun pel-erlang-skel-optional-separator (&optional percent char with-end)
  "Return a comment line separator if the end separators are used.
The comment uses PERCENT number of '%'.
The line is made with '-' unless another CHAR is specified.
If WITH-END is non-nil, return a Edoc @end statement before the separator
line (and even if no separator line is returned).
Return the same as `pel-erlang-skel-separator' if the
`pel-erlang-skel-use-secondary-separators' is non-nil otherwise return an empty
string.
Use this to create optional separator lines used by people that like them.
For those that prefer a lighter style these lines are not inserted."
  (concat (if with-end "%% @end\n" "")
          (if pel-erlang-skel-use-secondary-separators
              (pel-erlang-skel-separator percent char)
            "")))

(defun pel-erlang-skel-separator-start (&optional percent char)
  "Return a comment separator line if required by customized style.
The comment uses PERCENT number of '%'.
The line is made with '-' unless another CHAR is specified."
  (if pel-erlang-skel-use-separators
      (pel-erlang-skel-separator percent char)
    ""))

(defun pel-erlang-skel-separator-end (&optional percent char)
  "Return a comment end separator line if required by customized style.
The line length is set by `fill-column' with 70 being the smallest allowed.
The comment uses PERCENT number of '%'.
The line is made with '-' unless another CHAR is specified."
  (if (and pel-erlang-skel-use-separators
           pel-erlang-skel-use-secondary-separators)
      (concat "%% @end\n" (pel-erlang-skel-separator percent char))
    ""))

;; --
;; Extract Erlang function name & arguments

(defun pel-erlang-skel-get-function-name ()
  "Extract and return the name of the current Erlang function."
  (pel-with-required
      ('erlang)
      ('erlang-beginning-of-function
       'erlang-get-function-name)
      nil
    (save-excursion
      (erlang-beginning-of-function -1)
      (erlang-get-function-name))))

(defun pel-erlang-skel-get-function-args ()
  "Extract and return the arguments of the current Erlang function."
  (pel-with-required
      ('erlang)
      ('erlang-beginning-of-function
       'erlang-get-function-arguments)
      nil
    (save-excursion
      (erlang-beginning-of-function -1)
      (erlang-get-function-arguments))))

;; -----------------------------------------------------------------------------
;; Erlang Tempo Skeletons
;; ----------------------
;;
;; The tempo skeletons below are heavily based on the Erlang skeletons
;; provided by erlang-skels.el

(defvar pel-erlang-skel-export
  '(& "-export([" p "/"  n> "])." > n )
  "*The skeleton of a `export' declaration.
Please see the function `tempo-define-template'.")

(defvar pel-erlang-skel-import
  '((pel-skel-skip-blank) o >
    "-import(" (P "module: ") p ", [" p "/" n>
    "])." > n )
  "*The skeleton of a `import' declaration.
Please see the function `tempo-define-template'.")

(defvar pel-erlang-skel-try
  '((pel-skel-skip-blank) o >
    "try "  n>
    p   n>
    "catch" > n>
    p "oops         -> got_throw_oops;"     > n>
    p "throw:Other  -> {got_throw, Other};" > n>
    p "exit:Reason  -> {got_exit, Reason};" > n>
    p "error:Reason -> {got_error, Reason}" > n>
    "end." > n)
  "*The skeleton of a `try' expression.
Please see the function `tempo-define-template'.")

(defvar pel-erlang-skel-try-of
  '((pel-skel-skip-blank) o >
    "try "n>
    p n>
    " of" > n>
    > p " when " p " ->" >  n>
    p  n>
    "catch" > n>
    p "oops         when " p "  -> got_throw_oops;"     > n>
    p "throw:Other  when " p "  -> {got_throw, Other};" > n>
    p "exit:Reason  when " p "  -> {got_exit, Reason};" > n>
    p "error:Reason when " p "  -> {got_error, Reason}" > n>
    "after" > n>
    p  n>
    "end." > n)
  "*The skeleton of a `try' expression.
Please see the function `tempo-define-template'.")

(defvar pel-erlang-skel-function
  '((pel-erlang-skel-separator-start 2)
    "%% @doc " p n
    (pel-erlang-skel-separator-end 2)
    p "() " p "->"  n>
    p "." > n )
    "*The template of a function skeleton.
Please see the function `tempo-define-template'.")

(defvar pel-erlang-skel-spec
  '("-spec "
    (erlang-skel-get-function-name) "(" (erlang-skel-get-function-args)
    ") -> " p "undefined." n)
    "*The template of a -spec for the function following point.
Please see the function `tempo-define-template'.")

(defvar pel-skel-file-created
  '(& "%%% Created: " (pel-date) " by "
      (user-full-name) " <" erlang-skel-mail-address ">" n)
  "*The template for the \"Created:\" comment line.")

(defun pel-erlang-skel-filename ()
  "Insert name of current file."
  (concat "%%% File      : "
          (pel-current-buffer-filename :sans-directory)
          "\n"))

(defun pel-erlang-skel-maybe-timestamp (&optional event)
  "Insert time stamp if required."
  (when pel-erlang-skel-insert-file-timestamp
    (concat "%%% "
            (pel-time-stamp event "by ")
            "\n")))

(defvar pel-erlang-skel-file-doc-edoc
  '(& "%%% @doc " p n
      "%%%      " p n
      "%%% @end" n
      (pel-erlang-skel-separator)))

(defvar pel-erlang-skel-file-doc
  '(& "%%% Module Description:" n
      "%%% " n
      "%%% " p n
      "%%% " n
      (pel-erlang-skel-separator)))

(defun pel-erlang-skel-edoc-in-header-p ()
  "Return t if edoc must be used in header, nil otherwise."
  (eq pel-erlang-skel-with-edoc t))

(defun pel-erlang-skel-prompt-for-file-purpose ()
  "Return t if must prompt for file purpose, nil otherwise."
  (memq pel-erlang-skel-prompt-for-purpose '(t in-file-only)))

(defvar pel-skel-large-header
  '(o (pel-erlang-skel-optional-separator)
      (pel-erlang-skel-filename)
      (pel-skel-purpose-for (pel-erlang-skel-prompt-for-file-purpose)
                            "File" "%%%" "Purpose   :")
      (pel-skel-created-comment    "%%%")
      (pel-skel-author-comment     "%%%"
                                   (when (pel-erlang-skel-edoc-in-header-p)
                                     "@author"))
      (pel-erlang-skel-maybe-timestamp) ; this must be in the first 8 lines!
      (pel-skel-copyright-comment  "%%%"
                                   (when (pel-erlang-skel-edoc-in-header-p)
                                     "@copyright")
                                   pel-erlang-skel-with-license)
      (pel-erlang-skel-separator)
      ;; TODO: find a way to provide a pel-skel-include-if with 2 groups of
      ;;       skel forms. For now two mutually exclusive inclusions are used.
      (pel-skel-include-when (pel-erlang-skel-edoc-in-header-p) pel-erlang-skel-file-doc-edoc)
      (pel-skel-include-when (not (pel-erlang-skel-edoc-in-header-p)) pel-erlang-skel-file-doc)
      (pel-skel-include erlang-skel-small-header) )
  "*The template of a large header.
Please see the function `tempo-define-template'.")


;; Parts of behaviour templates.
(defvar pel-erlang-skel-internal-functions
  '((pel-erlang-skel-separator-start 3 ?=)
    "%%% Internal functions" n
    (pel-erlang-skel-optional-separator 3 ?=) n p n
    (pel-insert-line)))

(defun pel-erlang-skel-behaviour (behaviour api-funs callback-funs)
  "Return a BEHAVIOUR description string with API-FUNS and CALLBACK-FUNS."
  (concat
   (format "-behaviour(%s).\n\n" behaviour)
   (format "%%%% %s API\n"
           (pel-current-buffer-filename :sans-directory :sans-extension))
   (format "-export([%s]).\n\n" (pel-join api-funs
                                          ", "
                                          4 "         "))
   (format "%%%% %s callbacks\n" behaviour)
   (format "-export([%s]).\n\n" (pel-join callback-funs
                                          ", "
                                          4 "         "))
   "-define(SERVER, ?MODULE).\n\n"))

(defun pel-erlang-api (&optional suffix)
  "Return a API string with the name of the module."
  (format "%%%%%% %s API%s\n"
          (pel-current-buffer-filename :sans-directory :sans-extension)
          (if suffix
              (concat " "  suffix)
            "")))

;; Behaviour templates.
(defvar pel-erlang-skel-application
  '((pel-skel-include erlang-skel-large-header)
    "-behaviour(application)." n n
    "%% Application callbacks" n
    "-export([start/2, start_phase/3, stop/1, prep_stop/1," n>
    "config_change/3])." n n
    (pel-erlang-skel-separator-start 3 ?=)
    "%%% Application callbacks" n
    (pel-erlang-skel-optional-separator 3 ?=) n
    (pel-erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc  Start application." n
    "%%       Called when an application is started using application:start/[1,2]," n
    "%%       and application processes should be started." n
    "%%       If the application is structured according to the OTP design" n
    "%%       principles as a supervision tree, this means starting the" n
    "%%       top supervisor of the tree." n
    (pel-erlang-skel-optional-separator 2 ?- :with-end) n
    "-spec start(StartType :: normal |" n>
    "{takeover, Node :: node()} |" n>
    "{failover, Node :: node()}," n>
    "StartArgs :: term()) ->" n>
    "{ok, Pid :: pid()} |" n>
    "{ok, Pid :: pid(), State :: term()} |" n>
    "{error, Reason :: term()}." n
    "start(_StartType, _StartArgs) ->" n>
    "case 'TopSupervisor':start_link() of" n>
    "{ok, Pid} ->" n>
    "{ok, Pid};" n>
    "Error ->" n>
    "Error" n
    "end." > n
    n
    (pel-erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc  Top supervisor of the tree." n
    "%%       Starts an application with included applications, when" n
    "%%       synchronization is needed between processes in the different" n
    "%%       applications during startup." n
    (pel-erlang-skel-optional-separator 2 ?- :with-end) n
    "-spec start_phase(Phase :: atom()," n>
    "StartType :: normal |" n>
    "{takeover, Node :: node()} |" n>
    "{failover, Node :: node()}," n>
    "PhaseArgs :: term()) -> ok | {error, Reason :: term()}." n
    "start_phase(_Phase, _StartType, _PhaseArgs) ->" n>
    "ok."n
    n
    (pel-erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc  Handle application stop request." n
    "%%       Called whenever an application has stopped." n
    "%%       It is intended to be the opposite of Module:start/2 and should do" n
    "%%       any necessary cleaning up. The return value is ignored." n
    (pel-erlang-skel-optional-separator 2 ?- :with-end) n
    "-spec stop(State :: term()) -> any()." n
    "stop(_State) ->" n>
    "ok." n
    n
    (pel-erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc  Handle preparation of application stop." n
    "%%       Called when an application is about to be stopped," n
    "%%       before shutting down the processes of the application." n
    (pel-erlang-skel-optional-separator 2 ?- :with-end) n
    "-spec prep_stop(State :: term()) -> NewState :: term()." n
    "prep_stop(State) ->" n>
    "State." n
    n
    (pel-erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc  Handle code replacement configuration." n
    "%%       Called by an application after a code replacement," n
    "%%       if the configuration parameters have changed." n
    (pel-erlang-skel-optional-separator 2 ?- :with-end) n
    "-spec config_change(Changed :: [{Par :: atom(), Val :: term()}]," n>
    "New :: [{Par :: atom(), Val :: term()}]," n>
    "Removed :: [Par :: atom()]) -> ok." n
    "config_change(_Changed, _New, _Removed) ->" n>
    "ok." n
    n
    (pel-skel-include pel-erlang-skel-internal-functions)
    )
  "*The template of an application behaviour.
Please see the function `tempo-define-template'.")

(defvar pel-erlang-skel-generic-server
  '((pel-skel-include erlang-skel-large-header)
    (pel-erlang-skel-behaviour "gen_server"
                               '("start_link/0")
                               '("init/1"
                                 "handle_call/3"
                                 "handle_cast/2"
                                 "handle_info/2"
                                 "terminate/2"
                                 "code_change/3"
                                 "format_status/2"))
    "-record(state, {})." n n

    (pel-erlang-skel-separator-start 3 ?=)
    (pel-erlang-api)
    (pel-erlang-skel-optional-separator 3 ?=) n n

    (pel-erlang-skel-separator-start 2)
    "%% @doc  Start the server." n
    (pel-erlang-skel-optional-separator 2) n

    "-spec start_link() -> {ok, Pid :: pid()} |" n>
    "{error, Error :: {already_started, pid()}} |" n>
    "{error, Error :: term()} |" n>
    "ignore." n
    "start_link() ->" n>
    "gen_server:start_link({local, ?SERVER}, ?MODULE, [], [])." n
    n
    (pel-erlang-skel-separator-start 3 ?=)
    "%%% gen_server callbacks" n
    (pel-erlang-skel-optional-separator 3 ?=) n

    (pel-erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc  Initialize the server." n
    (pel-erlang-skel-optional-separator 2) n
    "-spec init(Args :: term()) -> {ok, State :: term()} |" n>
    "{ok, State :: term(), Timeout :: timeout()} |" n>
    "{ok, State :: term(), hibernate} |" n>
    "{stop, Reason :: term()} |" n>
    "ignore." n
    "init([]) ->" n>
    "process_flag(trap_exit, true)," n>
    "{ok, #state{}}." n
    n
    (pel-erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc  Handle call messages." n
    (pel-erlang-skel-optional-separator 2) n
    "-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->" n>
    "{reply, Reply :: term(), NewState :: term()} |" n>
    "{reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |" n>
    "{reply, Reply :: term(), NewState :: term(), hibernate} |" n>
    "{noreply, NewState :: term()} |" n>
    "{noreply, NewState :: term(), Timeout :: timeout()} |" n>
    "{noreply, NewState :: term(), hibernate} |" n>
    "{stop, Reason :: term(), Reply :: term(), NewState :: term()} |" n>
    "{stop, Reason :: term(), NewState :: term()}." n
    "handle_call(_Request, _From, State) ->" n>
    "Reply = ok," n>
    "{reply, Reply, State}." n
    n
    (pel-erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc  Handle cast messages." n
    (pel-erlang-skel-optional-separator 2) n
    "-spec handle_cast(Request :: term(), State :: term()) ->" n>
    "{noreply, NewState :: term()} |" n>
    "{noreply, NewState :: term(), Timeout :: timeout()} |" n>
    "{noreply, NewState :: term(), hibernate} |" n>
    "{stop, Reason :: term(), NewState :: term()}." n
    "handle_cast(_Request, State) ->" n>
    "{noreply, State}." n
    n
    (pel-erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc  Handle all non call/cast messages." n
    (pel-erlang-skel-optional-separator 2) n
    "-spec handle_info(Info :: timeout() | term(), State :: term()) ->" n>
    "{noreply, NewState :: term()} |" n>
    "{noreply, NewState :: term(), Timeout :: timeout()} |" n>
    "{noreply, NewState :: term(), hibernate} |" n>
    "{stop, Reason :: normal | term(), NewState :: term()}." n
    "handle_info(_Info, State) ->" n>
    "{noreply, State}." n
    n
    (pel-erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc  Terminate." n
    "%%       Called by a gen_server when it is about to terminate." n
    "%%       It should be the opposite of Module:init/1 and do any" n
    "%%       necessary cleaning up." n
    "%%       When it returns, the gen_server terminates with Reason." n
    "%%       The return value is ignored." n
    (pel-erlang-skel-optional-separator 2 ?- :with-end) n
    "-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term()," n>
    "State :: term()) -> any()." n
    "terminate(_Reason, _State) ->" n>
    "ok." n
    n
    (pel-erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc  Convert process state when code is changed." n
    (pel-erlang-skel-optional-separator 2) n
    "-spec code_change(OldVsn :: term() | {down, term()}," n>
    "State :: term()," n>
    "Extra :: term()) -> {ok, NewState :: term()} |" n>
    "{error, Reason :: term()}." n
    "code_change(_OldVsn, State, _Extra) ->" n>
    "{ok, State}." n
    n
    (pel-erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc  Handle status change request." n
    "%%       Called for changing the form and appearance of gen_server" n
    "%%       server status when it is returned from sys:get_status/1,2" n
    "%%       or when it appears in termination error logs." n
    (pel-erlang-skel-optional-separator 2 ?- :with-end) n
    "-spec format_status(Opt :: normal | terminate," n>
    "Status :: list()) -> Status :: term()." n
    "format_status(_Opt, Status) ->" n>
    "Status." n
    n
    (pel-skel-include pel-erlang-skel-internal-functions)
    )
  "*The template of a generic server.
Please see the function `tempo-define-template'.")

(defvar pel-erlang-skel-supervisor
  '((pel-skel-include erlang-skel-large-header)
    (pel-erlang-skel-behaviour "supervisor"
                               '("start_link/0")
                               '("init/1"))

    (pel-erlang-skel-separator-start 3 ?=)
    (pel-erlang-api "functions")
    (pel-erlang-skel-optional-separator 3 ?=) n
    (pel-erlang-skel-separator-start 2)
    "%% @doc  Start the supervisor." n
    (pel-erlang-skel-optional-separator 2) n
    "-spec start_link() -> {ok, Pid :: pid()} |" n>
    "{error, {already_started, Pid :: pid()}} |" n>
    "{error, {shutdown, term()}} |" n>
    "{error, term()} |" n>
    "ignore." n
    "start_link() ->" n>
    "supervisor:start_link({local, ?SERVER}, ?MODULE, [])." n
    n
    (pel-erlang-skel-separator-start 3 ?=)
    "%%% Supervisor callbacks" n
    (pel-erlang-skel-optional-separator 3 ?=) n
    (pel-erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc  Initialize the server." n
    "%%       Called by the new process when a supervisor is started" n
    "%%       using supervisor:start_link/[2,3] to find out about" n
    "%%       restart strategy, maximum restart intensity, and child" n
    "%%       specifications." n
    (pel-erlang-skel-optional-separator 2 ?- :with-end) n
    "-spec init(Args :: term()) ->" n>
    "{ok, {SupFlags :: supervisor:sup_flags()," n>
    "[ChildSpec :: supervisor:child_spec()]}} |" n>
    "ignore." n
    "init([]) ->" n
    "" n>
    "SupFlags = #{strategy => one_for_one," n>
    "intensity => 1," n>
    "period => 5}," n
    "" n>
    "AChild = #{id => 'AName'," n>
    "start => {'AModule', start_link, []}," n>
    "restart => permanent," n>
    "shutdown => 5000," n>
    "type => worker," n>
    "modules => ['AModule']}," n
    "" n>
    "{ok, {SupFlags, [AChild]}}." n
    n
    (pel-skel-include pel-erlang-skel-internal-functions)
    )
  "*The template of a supervisor behaviour.
Please see the function `tempo-define-template'.")

(defvar pel-erlang-skel-supervisor-bridge
  '((pel-skel-include erlang-skel-large-header)
    (pel-erlang-skel-behaviour "supervisor_bridge"
                               '("start_link/0")
                               '("init/1" "terminate/2"))
    "-record(state, {})." n n

    (pel-erlang-skel-separator-start 3 ?=)
    (pel-erlang-api)
    (pel-erlang-skel-optional-separator 3 ?=) n
    (pel-erlang-skel-separator-start 2)
    "%% @doc  Start the supervisor bridge." n
    (pel-erlang-skel-optional-separator 2) n
    "-spec start_link() -> {ok, Pid :: pid()} |" n>
    "{error, {already_started, Pid :: pid()}} |" n>
    "{error, term()} |" n>
    "ignore." n
    "start_link() ->" n>
    "supervisor_bridge:start_link({local, ?SERVER}, ?MODULE, [])." n
    n
    (pel-erlang-skel-separator-start 3 ?=)
    "%%% supervisor_bridge callbacks" n
    (pel-erlang-skel-optional-separator 3 ?=) n
    (pel-erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc  Initialize the supervisor bridge." n
    "%%       Create a supervisor_bridge process, linked to the calling process," n
    "%%       which calls Module:init/1 to start the subsystem. To ensure a" n
    "%%       synchronized start-up procedure, this function does not return" n
    "%%       until Module:init/1 has returned." n
    (pel-erlang-skel-optional-separator 2) n
    "-spec init(Args :: term()) -> {ok, Pid :: pid(), State :: term()} |" n>
    "{error, Error :: term()} |" n>
    "ignore." n
    "init([]) ->" n>
    "case 'AModule':start_link() of" n>
    "{ok, Pid} ->" n>
    "{ok, Pid, #state{}};" n>
    "Error ->" n>
    "Error" n
    "end." > n
    n
    (pel-erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc  Terminate." n
    "%%       Called by the supervisor_bridge when it is about to terminate." n
    "%%       It should be the opposite of Module:init/1 and stop" n
    "%%       the subsystem and do any necessary cleaning up." n
    "%%       The return value is ignored." n
    (pel-erlang-skel-optional-separator 2) n
    "-spec terminate(Reason :: shutdown | term(), State :: term()) -> any()." n
    "terminate(_Reason, _State) ->" n>
    "'AModule':stop()," n>
    "ok." n
    n
    (pel-skel-include pel-erlang-skel-internal-functions)
    )
  "*The template of a supervisor_bridge behaviour.
Please see the function `tempo-define-template'.")

(defvar pel-erlang-skel-gen-event
  '((pel-skel-include erlang-skel-large-header)
    (pel-erlang-skel-behaviour "gen_event"
                               '("start_link/0"
                                 "add_handler/0")
                               '("init/1"
                                 "handle_event/2"
                                 "handle_call/2"
                                 "handle_info/2"
                                 "terminate/2"
                                 "code_change/3"
                                 "format_status/2"))
    "-record(state, {})." n n

    (pel-erlang-skel-separator-start 3 ?=)
    (pel-erlang-api)
    (pel-erlang-skel-optional-separator 3 ?=) n
    (pel-erlang-skel-separator-start 2)
    "%% @doc  Create an event manager." n
    (pel-erlang-skel-optional-separator 2) n
    "-spec start_link() -> {ok, Pid :: pid()} |" n>
    "{error, Error :: {already_started, pid()} | term()}." n
    "start_link() ->" n>
    "gen_event:start_link({local, ?SERVER})." n
    n
    (pel-erlang-skel-separator-start 2)
    "%% @doc  Add an event handler." n
    (pel-erlang-skel-optional-separator 2) n
    "-spec add_handler() -> ok | {'EXIT', Reason :: term()} | term()." n
    "add_handler() ->" n>
    "gen_event:add_handler(?SERVER, ?MODULE, [])." n
    n
    (pel-erlang-skel-separator-start 3 ?=)
    "%%% gen_event callbacks" n
    (pel-erlang-skel-optional-separator 3 ?=) n
    (pel-erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc  Initialize the event handler." n
    "%%       Whenever a new event handler is added to an event manager," n
    "%%       this function is called to initialize the event handler." n
    (pel-erlang-skel-optional-separator 2 ?- :with-end) n
    "-spec init(Args :: term() | {Args :: term(), Term :: term()}) ->" n>
    "{ok, State :: term()} |" n>
    "{ok, State :: term(), hibernate} |" n>
    "{error, Reason :: term()}." n
    "init([]) ->" n>
    "{ok, #state{}}." n
    n
    (pel-erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc  Handle event." n
    "%%       Called whenever an event manager receives an event sent using" n
    "%%       gen_event:notify/2 or gen_event:sync_notify/2, it is" n
    "%%       called for each installed event handler to handle the event." n
    (pel-erlang-skel-optional-separator 2 ?- :with-end) n
    "-spec handle_event(Event :: term(), State :: term()) ->" n>
    "{ok, NewState :: term()} |" n>
    "{ok, NewState :: term(), hibernate} |" n>
    "remove_handler |" n>
    "{swap_handler, Args1 :: term(), NewState :: term()," n>
    "Handler2 :: atom() | {atom(), term()} , Args2 :: term()}." n>
    "handle_event(_Event, State) ->" n>
    "{ok, State}." n
    n
    (pel-erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc  Handle request." n
    "%%       Called when an event manager receives a request sent using" n
    "%%       gen_event:call/3,4. It's called  for the specified" n
    "%%       event handler to handle the request." n
    (pel-erlang-skel-optional-separator 2 ?- :with-end) n
    "-spec handle_call(Request :: term(), State :: term()) ->" n>
    "{ok, Reply :: term(), NewState :: term()} |" n>
    "{ok, Reply :: term(), NewState :: term(), hibernate} |" n>
    "{remove_handler, Reply :: term()} |" n>
    "{swap_handler, Reply :: term(), Args1 :: term(), NewState :: term()," n>
    "Handler2 :: atom() | {atom(), term()}, Args2 :: term()}." n
    "handle_call(_Request, State) ->" n>
    "Reply = ok," n>
    "{ok, Reply, State}." n
    n
    (pel-erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc  Handle info." n
    "%%       Called for each installed event handler when" n
    "%%       an event manager receives any other message than an event or a" n
    "%%       synchronous request (or a system message)." n
    (pel-erlang-skel-optional-separator 2 ?- :with-end) n
    "-spec handle_info(Info :: term(), State :: term()) ->" n>
    "{ok, NewState :: term()} |" n>
    "{ok, NewState :: term(), hibernate} |" n>
    "remove_handler |" n>
    "{swap_handler, Args1 :: term(), NewState :: term()," n>
    "Handler2 :: atom() | {atom(), term()}, Args2 :: term()}." n
    "handle_info(_Info, State) ->" n>
    "{ok, State}." n
    n
    (pel-erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc  Terminate." n
    "%%       Called when an event handler is deleted from an event manager." n
    "%%       It should be the opposite of Module:init/1 and" n
    "%%       do any necessary cleaning up." n
    (pel-erlang-skel-optional-separator 2 ?- :with-end) n
    "-spec terminate(Arg :: {stop, Reason :: term()} |" n>
    "stop |" n>
    "remove_handler |" n>
    "{error, {'EXIT', Reason :: term()}} |" n>
    "{error, Term :: term()} |" n>
    "term()," n>
    "State :: term()) -> any()." n
    "terminate(_Arg, _State) ->" n>
    "ok." n
    n
    (pel-erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc  Convert process state when code is changed." n
    (pel-erlang-skel-optional-separator 2) n
    "-spec code_change(OldVsn :: term() | {down, term()}," n>
    "State :: term()," n>
    "Extra :: term()) -> {ok, NewState :: term()}." n
    "code_change(_OldVsn, State, _Extra) ->" n>
    "{ok, State}." n
    n
    (pel-erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc  Handle status change request." n
    "%%       Called for changing the form and appearance" n
    "%%       of gen_event status when it is returned from sys:get_status/1,2" n
    "%%       or when it appears in termination error logs." n
    (pel-erlang-skel-optional-separator 2 ?- :with-end) n
    "-spec format_status(Opt :: normal | terminate," n>
    "Status :: list()) -> Status :: term()." n
    "format_status(_Opt, Status) ->" n>
    "Status." n
    n
    (pel-skel-include pel-erlang-skel-internal-functions)
    )
  "*The template of a gen_event.
Please see the function `tempo-define-template'.")

;; TODO: replace @spec with -spec declarations.
(defvar pel-erlang-skel-gen-fsm
  '((erlang-skel-include erlang-skel-large-header)
    (pel-erlang-skel-behaviour "gen_fsm"
                               '("start_link/0")
                               '("init/1"
                                 "state_name/2"
                                 "state_name/3"
                                 "handle_event/3"
                                 "handle_sync_event/4"
                                 "handle_info/3"
                                 "terminate/3"
                                 "code_change/4"))
    "-record(state, {})." n n

    (pel-erlang-skel-separator-start 3 ?=)
    (pel-erlang-api)
    (pel-erlang-skel-optional-separator 3 ?=) n
    (pel-erlang-skel-separator-start 2)
    "%% @doc  Start and link." n
    "%%       Create a gen_fsm process which calls Module:init/1 to" n
    "%%       initialize. To ensure a synchronized start-up procedure, this" n
    "%%       function does not return until Module:init/1 has returned." n
    "%%" n
    "%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}" n
    (pel-erlang-skel-optional-separator 2) n
    "start_link() ->" n>
    "gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], [])." n
    n
    (pel-erlang-skel-separator-start 3 ?=)
    "%%% gen_fsm callbacks" n
    (pel-erlang-skel-optional-separator 3 ?=) n
    (pel-erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc  Initialize." n
    "%%       Whenever a gen_fsm is started using gen_fsm:start/[3,4] or" n
    "%%       gen_fsm:start_link/[3,4], this function is called by the new" n
    "%%       process to initialize." n
    "%%" n
    "%% @spec init(Args) -> {ok, StateName, State} |" n
    "%%                     {ok, StateName, State, Timeout} |" n
    "%%                     ignore |" n
    "%%                     {stop, StopReason}" n
    (pel-erlang-skel-optional-separator 2) n
    "init([]) ->" n>
    "process_flag(trap_exit, true)," n>
    "{ok, state_name, #state{}}." n
    n
    (pel-erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc  Handle event at state." n
    "%%       There should be one instance of this function for each possible" n
    "%%       state name." n
    "%%       Whenever a gen_fsm receives an event sent using" n
    "%%       gen_fsm:send_event/2, the instance of this function with the same" n
    "%%       name as the current state name StateName is called to handle" n
    "%%       the event. It is also called if a timeout occurs." n
    "%%" n
    "%% @spec state_name(Event, State) ->" n
    "%%                   {next_state, NextStateName, NextState} |" n
    "%%                   {next_state, NextStateName, NextState, Timeout} |" n
    "%%                   {stop, Reason, NewState}" n
    (pel-erlang-skel-optional-separator 2) n
    "state_name(_Event, State) ->" n>
    "{next_state, state_name, State}." n
    n
    (pel-erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc  Handle event at state." n
    "%%       There should be one instance of this function for each possible" n
    "%%       state name." n
    "%%       Whenever a gen_fsm receives an event sent using" n
    "%%       gen_fsm:sync_send_event/[2,3], the instance of this function with" n
    "%%       the same name as the current state name StateName is called to" n
    "%%       handle the event." n
    "%%" n
    "%% @spec state_name(Event, From, State) ->" n
    "%%                   {next_state, NextStateName, NextState} |"n
    "%%                   {next_state, NextStateName, NextState, Timeout} |" n
    "%%                   {reply, Reply, NextStateName, NextState} |" n
    "%%                   {reply, Reply, NextStateName, NextState, Timeout} |" n
    "%%                   {stop, Reason, NewState} |" n
    "%%                   {stop, Reason, Reply, NewState}" n
    (pel-erlang-skel-optional-separator 2) n
    "state_name(_Event, _From, State) ->" n>
    "Reply = ok," n>
    "{reply, Reply, state_name, State}." n
    n
    (pel-erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc Handle event." n
    "%%      Whenever a gen_fsm receives an event sent using" n
    "%%      gen_fsm:send_all_state_event/2, this function is called to handle" n
    "%%      the event." n
    "%%" n
    "%% @spec handle_event(Event, StateName, State) ->" n
    "%%                   {next_state, NextStateName, NextState} |" n
    "%%                   {next_state, NextStateName, NextState, Timeout} |" n
    "%%                   {stop, Reason, NewState}" n
    (pel-erlang-skel-optional-separator 2) n
    "handle_event(_Event, StateName, State) ->" n>
    "{next_state, StateName, State}." n
    n
    (pel-erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc  Handle synchronous event." n
    "%%       Whenever a gen_fsm receives an event sent using" n
    "%%       gen_fsm:sync_send_all_state_event/[2,3], this function is called" n
    "%%       to handle the event." n
    "%%" n
    "%% @spec handle_sync_event(Event, From, StateName, State) ->" n
    "%%                   {next_state, NextStateName, NextState} |" n
    "%%                   {next_state, NextStateName, NextState, Timeout} |" n
    "%%                   {reply, Reply, NextStateName, NextState} |" n
    "%%                   {reply, Reply, NextStateName, NextState, Timeout} |" n
    "%%                   {stop, Reason, NewState} |" n
    "%%                   {stop, Reason, Reply, NewState}" n
    (pel-erlang-skel-optional-separator 2) n
    "handle_sync_event(_Event, _From, StateName, State) ->" n>
    "Reply = ok," n>
    "{reply, Reply, StateName, State}." n
    n
    (pel-erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc  Handle info." n
    "%%       This function is called by a gen_fsm when it receives any" n
    "%%       message other than a synchronous or asynchronous event" n
    "%%       (or a system message)." n
    "%%" n
    "%% @spec handle_info(Info,StateName,State)->" n
    "%%                   {next_state, NextStateName, NextState} |" n
    "%%                   {next_state, NextStateName, NextState, Timeout} |" n
    "%%                   {stop, Reason, NewState}" n
    (pel-erlang-skel-optional-separator 2) n
    "handle_info(_Info, StateName, State) ->" n>
    "{next_state, StateName, State}." n
    n
    (pel-erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc Terminate." n
    "%%      This function is called by a gen_fsm when it is about to" n
    "%%      terminate. It should be the opposite of Module:init/1 and do any" n
    "%%      necessary cleaning up. When it returns, the gen_fsm terminates with" n
    "%%      Reason. The return value is ignored." n
    "%%" n
    "%% @spec terminate(Reason, StateName, State) -> void()" n
    (pel-erlang-skel-optional-separator 2) n
    "terminate(_Reason, _StateName, _State) ->" n>
    "ok." n
    n
    (pel-erlang-skel-separator-start 2)
    "%% @private" n
    "%% @doc  Handle code change." n
    "%%       Convert process state when code is changed" n
    "%%" n
    "%% @spec code_change(OldVsn, StateName, State, Extra) ->" n
    "%%                   {ok, StateName, NewState}" n
    (pel-erlang-skel-optional-separator 2) n
    "code_change(_OldVsn, StateName, State, _Extra) ->" n>
    "{ok, StateName, State}." n
    n
    (pel-skel-include pel-erlang-skel-internal-functions)
    )
  "*The template of a gen_fsm.
Please see the function `tempo-define-template'.")

;; -----------------------------------------------------------------------------
;; Installation of Erlang Tempo Skeletons
;; --------------------------------------
;;
;; The code in this section improves the official Erlang skeletons and assign
;; key bindings for them.  The Erlang skeletons provided here are improvements
;; on the official Erlang skeletons with provided customization and extended
;; tempo marks and Erlang code to simplify and enhance writing Erlang code.
;; The code below installs replacement inside the erlang.el where needed.
;;
;; Ideally this code would be incorporated inside the official erlang.el library.
;; I might try to do that once this code stabilizes.


;; -------
;; Install Erlang Skeletons as key-bound commands
;;
;; Add extra tempo skeletons for Erlang to complement what erlang.el already
;; have, add key bindings to allow inserting the template text with a keystroke
;; and provide a minor mode to help navigating inside the template.

(defvar pel--erl-skel-key '(("if"                      . "i")
                            ("case"                    . "c")
                            ("export"                  . "x")
                            ("import"                  . "I")
                            ("try"                     . "t")
                            ("try-of"                  . "T")
                            ("receive"                 . "r")
                            ("after"                   . "a")
                            ("loop"                    . "l")
                            ("module"                  . "m")
                            ("function"                . "f")
                            ("author"                  . "`")
                            ("spec"                    . "s")
                            ("small-header"              "M-h" pel-skel-header)
                            ("normal-header"             "M-H" pel-skel-header)
                            ("large-header"              "C-h" pel-skel-header)
                            ("small-server"              "M-s" pel-skel-header)
                            ("application"               "M-a" pel-skel-header)
                            ("supervisor"                "M-u" pel-skel-header)
                            ("supervisor-bridge"         "M-b" pel-skel-header)
                            ("generic-server"            "M-g" pel-skel-header)
                            ("gen-event"                 "M-e" pel-skel-header)
                            ("gen-fsm"                   "M-f" pel-skel-header)
                            ("gen-statem-StateName"      "M-S" pel-skel-header)
                            ("gen-statem-handle-event"   "M-E" pel-skel-header)
                            ("wx-object"                 "M-w" pel-skel-header)
                            ("gen-lib"                   "M-l" pel-skel-header)
                            ("gen-corba-cb"              "M-c" pel-skel-header)
                            ("ct-test-suite-s"           "M-1" pel-skel-header)
                            ("ct-test-suite-l"           "M-2" pel-skel-header)
                            ("ts-test-suite"             "M-3" pel-skel-header))
  "Key mapping for skeletons defined in erlang-skel.el
Each element of the list has one of the 2 following forms:
- a 2 element (name . key) cons cell,
- a 3 element (name key preliminary-function) list.
The first element is always the template name.
The second element is always a key sequence string.
The third element is optional. If present, it is the
symbol of a preparation function to call with the tempo skeleton code.
That's often the `pel-skel-header' used to insert the skeleton at
the beginning of the buffer instead of at point, the default.")

;; The standard Erlang mode support does not define skeleton for all statements.
;; Add more skeletons using the tempo package here to complement the official ones.

;; Functions used inside the skeleton descriptions below.

(defvar pel--more-erlang-skel
  '(("Export"    "export"    pel-erlang-skel-export)
    ("Import"    "import"    pel-erlang-skel-import)
    ("Try"       "try"       pel-erlang-skel-try)
    ("Try-of"    "try-of"    pel-erlang-skel-try-of))
  "Extra template entries to inserted by PEL inside `erlang-skel'.")

(defun pel--update-erlang-skel ()
  "Update the list of Erlang skeletons."
    (when (and (require 'erlang nil :noerror)
           (boundp 'erlang-skel-file)
           (load erlang-skel-file :noerror)
           (boundp 'erlang-skel)
           (boundp 'erlang-skel-function)
           (boundp 'erlang-skel-created-comment)
           (boundp 'erlang-skel-large-header)
           (boundp 'erlang-skel-application)
           (boundp 'erlang-skel-generic-server)
           (boundp 'erlang-skel-supervisor)
           (boundp 'erlang-skel-supervisor-bridge)
           (boundp 'erlang-skel-generic-server)
           (boundp 'erlang-skel-gen-event)
           (boundp 'erlang-skel-gen-fsm)
           (fboundp 'erlang-skel-separator))
      ;; Update some Erlang skeletons with more flexible ones
      ;; - separator line length controlled by `fill-column'
      (fset 'erlang-skel-separator      'pel-erlang-skel-separator)
      ;; - function skel: second separator line is optional
      (setq erlang-skel-function        pel-erlang-skel-function)
      ;; time uses a YYYY-MM-DD format
      (setq erlang-skel-created-comment pel-skel-file-created)
      ;; large header:
      ;; The separator on the first line is optional.
      ;; The second line shows the file name.
      ;; Writes all date/time at the top, with an optional
      ;; auto-updated timestamp (must be in the first 8 lines)
      ;; Edoc text at the end of block, has marks and is indented.
      ;; It also expects the first @doc line to be a self-contained abstract.
      (setq erlang-skel-large-header    pel-skel-large-header)
      ;; Behaviours:
      ;; - second separator lines are optional.
      ;; - name of module is shown before API
      ;; - doc descriptions have a header line that's a full sentence.
      (setq erlang-skel-application       pel-erlang-skel-application)
      (setq erlang-skel-generic-server    pel-erlang-skel-generic-server)
      (setq erlang-skel-supervisor        pel-erlang-skel-supervisor)
      (setq erlang-skel-supervisor-bridge pel-erlang-skel-supervisor-bridge)
      (setq erlang-skel-generic-server    pel-erlang-skel-generic-server)
      (setq erlang-skel-gen-event         pel-erlang-skel-gen-event)
      (setq erlang-skel-gen-fsm           pel-erlang-skel-gen-fsm)
      ;; Install the extra skeletons inside the erlang.el list of skeletons:
      ;; the list erlang-skel
      (setq erlang-skel (pel-insert-list-in-list
                         pel--more-erlang-skel 2 erlang-skel))))

;;-pel-autoload
(defun pel--erlang-mode-setup ()
  "Provide extra functionality to the Erlang mode.
Add extra tempo templates.
This function is meant to be used as an `advice-add'
to execute *before* `erlang-mode'."
  (pel--update-erlang-skel))

;; --

;;-pel-autoload
(defun pel--install-erlang-skel (key-map)
  "Create PEL Erlang skeleton functions and bind them in the KEY-MAP specified.
This function is meant to be called by pel-init() only."
  (if (and (require 'erlang nil :noerror)
               (boundp 'erlang-skel))
      (pel-tempo-install-pel-skel
       "erlang" erlang-skel key-map pel--erl-skel-key "erl")
    (user-error "The erlang.el package is not loaded!")))

;; -----------------------------------------------------------------------------
(provide 'pel-erlang-skels)

;;; pel-erlang-skels.el ends here
