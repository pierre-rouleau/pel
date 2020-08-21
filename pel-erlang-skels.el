;;; pel-erlang-skels.el --- Erlang specific tempo skeletons  -*- lexical-binding: t; -*-

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
;;
;; Functions ('-') and variables ('>') used in Erlang tempo skeletons:

;; Line separators:
;;     - pel-erlang-skel-separator
;;     - pel-erlang-skel-optional-separator
;;     - pel-erlang-skel-separator-start
;;     - pel-erlang-skel-separator-end

;; Extract Erlang function name & arguments:
;;     - pel-erlang-skel-get-function-name
;;     - pel-erlang-skel-get-function-args

;; Erlang Tempo Skeletons:
;;   > pel-erlang-skel-export
;;   > pel-erlang-skel-import
;;   > pel-erlang-skel-try
;;   > pel-erlang-skel-try-of
;;   > pel-erlang-skel-function
;;     - pel--erlang-skel-function
;;       - pel-prompt-erlang-function
;;   > pel-erlang-skel-spec
;;   > pel-skel-file-created

;; Formatting functions and predicates, based on user options:
;;     - pel-erlang-skel-maybe-timestamp
;;     - pel-erlang-skel-edoc-in-header-p
;;     - pel-erlang-skel-edoc-in-function-p
;;     - pel-erlang-skel-prompt-for-file-purpose-p
;;     - pel-erlang-skel-prompt-for-function-purpose-p
;;     - pel-erlang-skel-prompt-for-function-name-p

;; Tempo skeleton for large file header:
;;   > pel-skel-large-header
;;     - pel-erlang-skel-filename
;;     - pel-erlang-skel-file-doc

;; Tempo skeletons for OTP behaviours:
;;   > pel-erlang-skel-application
;;   > pel-erlang-skel-generic-server
;;   > pel-erlang-skel-supervisor
;;   > pel-erlang-skel-supervisor-bridge
;;   > pel-erlang-skel-gen-event
;;   > pel-erlang-skel-gen-fsm
;;   > pel-erlang-skel-gen-statem-StateName
;;   > pel-erlang-skel-gen-statem-handle-event
;;     - pel-erlang-skel-behaviour
;;     - pel-erlang-skel-api-block
;;     - pel-erlang-skel-function-doc

;; Installation of Erlang Tempo Skeletons:
;;   > pel--erl-skel-key
;;   > pel--more-erlang-skel
;;   - pel--update-erlang-skel
;;   - pel--erlang-mode-setup
;;   - pel--install-erlang-skel

;; -----------------------------------------------------------------------------
;;; Dependencies:

(require 'pel--base)          ; use: pel-current-buffer-filename
;;                            ;      pel-hastext
(require 'pel--options)       ; use: pel-erlang-skel-use-separators
;;                            ;      pel-erlang-skel-use-secondary-separators
(require 'pel--macros)
(require 'pel-list)           ; use: pel-insert-list-in-list, pel-join
(require 'pel-tempo)          ; use: pel-tempo-mode,
;;                            ;      pel-tempo-install-pel-skel
(require 'pel-skels)

;; -----------------------------------------------------------------------------
;;; Code:

;; --
;; Line separators


(defun pel-erlang-skel-separator (&optional percent char)
  "Return a comment separator line of `fill-column' length.
The comment uses PERCENT number of '%' (or 3 if not specified).
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
      (concat (if pel-erlang-skel-with-edoc
                  "%% @end\n"
                "")
              (pel-erlang-skel-separator percent char))
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

;; --
;; Insert an Erlang function

(defun pel--erlang-transform (text)
  "Transform function name and argument names passed in TEXT.
Replaces dash by underscore."
  (replace-regexp-in-string "-" "_" text))

(defun pel-prompt-erlang-function ()
  "Prompt for an Erlang function.
Replace dash by underscore in typed name.
Maintain and prompt history."
  (pel-prompt-function (function pel--erlang-transform)))

(defun pel-prompt-erlang-args ()
  "Prompt for Erlang function arguments.
Expect comma-separated arguments.
Replace dash by underscore in typed name.
Maintain and prompt history."
  (pel-prompt-args (function pel--erlang-transform)))

(defun pel--erlang-skel-function ()
  "Return a skeleton list for Erlang function.
Prompt for function name and purpose if specified by customization.
Insert Edoc markup if specified by customization."
  (let* ((sk (list 'l))
         (fname       (if pel-erlang-skel-prompt-for-function-name
                          (pel-prompt-erlang-function)
                        ""))
         (args        (if pel-erlang-skel-prompt-for-function-arguments
                          (pel-prompt-erlang-args)
                        ""))
         (purpose     (if (pel-erlang-skel-prompt-for-function-purpose-p)
                          (pel-prompt-purpose-for "Function")
                        ""))
         (fname-val   (if (pel-hastext fname) fname 'p))
         (purpose-val (if (pel-hastext purpose) purpose 'p)))
    (pel-append-to sk (list (pel-erlang-skel-separator-start 2)))
    ;; description
    (if (pel-erlang-skel-edoc-in-function-p)
        ;; with Edoc
        (if (string= purpose "")
            (pel-append-to sk (list "%% @doc " 'p 'n))
          (pel-append-to sk (list (concat "%% @doc " purpose) 'n)))
      ;; without Edoc
      (pel-append-to sk (list "%% " fname-val " : " purpose-val 'n)))
    (pel-append-to sk (list (pel-erlang-skel-optional-separator
                             2
                             ?-
                             (pel-erlang-skel-edoc-in-function-p))))
    (pel-append-to sk (list
                       'n
                       "-spec " fname-val "(" args ") -> " 'p "." 'n 'n
                       fname-val "(" args ") -> " 'n> 'p "." 'n))))

(defvar pel-erlang-skel-function
  '((pel--erlang-skel-function))
    "The template of a function skeleton.")
;; --

(defvar pel-erlang-skel-spec
  '("-spec "
    (pel-erlang-skel-get-function-name) "(" (pel-erlang-skel-get-function-args)
    ") -> " p "undefined." n)
    "*The template of a -spec for the function following point.
Please see the function `tempo-define-template'.")

(defvar pel-skel-file-created
  '(& "%%% Created: " (pel-date) " by "
      (user-full-name) " <" erlang-skel-mail-address ">" n)
  "*The template for the \"Created:\" comment line.")

;; -----------------------------------------------------------------------------
;; Formatting predicates, based on user options.

(defun pel-erlang-skel-maybe-timestamp (&optional event)
  "Insert time stamp foe EVENT if required."
  (when pel-erlang-skel-insert-file-timestamp
    (concat "%%% "
            (pel-time-stamp event "by ")
            "\n")))

(defun pel-erlang-skel-edoc-in-header-p ()
  "Return t if edoc must be used in header, nil otherwise."
  (eq pel-erlang-skel-with-edoc t))

(defun pel-erlang-skel-edoc-in-function-p ()
  "Return t if edoc must be used in header, nil otherwise."
  (memq pel-erlang-skel-with-edoc '(t in-function-only)))

(defun pel-erlang-skel-prompt-for-file-purpose-p ()
  "Return t if must prompt for file purpose, nil otherwise."
  (memq pel-erlang-skel-prompt-for-purpose '(t in-file-only)))

(defun pel-erlang-skel-prompt-for-function-purpose-p ()
  "Return t if must prompt for file purpose, nil otherwise."
  (memq pel-erlang-skel-prompt-for-purpose '(t in-function-only)))

(defalias 'pel-erlang-skel-prompt-for-function-name-p
  'pel-erlang-skel-prompt-for-function-purpose-p
  "Return t if must prompt for file name, nil otherwise.")

;; -----------------------------------------------------------------------------
;; Tempo skeleton for large file header

(defun pel-erlang-skel-file-doc ()
  "Return tempo skel for documentation ready for insertion.
The returned skel list uses EDoc style if EDoc must be used,
and inserts the last specified purpose string as the single
line description if it was identified."
  (if (pel-erlang-skel-edoc-in-header-p)
      ;; with Edoc
      (if (string= pel--skel-last-purpose "")
          (cons 'l
                '(& "%%% @doc " p n
                    "%%%      " p n
                    "%%% @end" n
                    (pel-erlang-skel-separator 3 ?=)))
        (cons 'l
              '(& (concat "%%% @doc " pel--skel-last-purpose) n
                  "%%%      " p n
                  "%%% @end" n
                  (pel-erlang-skel-separator 3 ?=))))
    ;; without Edoc
    (if (string= pel--skel-last-purpose "")
        (cons 'l
              '(& "%%% Module Description:" n p
                  "%%% " n
                  "%%% " p n
                  "%%% " n
                  (pel-erlang-skel-separator 3 ?=)))
      (cons 'l
            '(& (concat "%%% Module Description: " pel--skel-last-purpose) n
                "%%% " n
                "%%% " p n
                (pel-erlang-skel-separator 3 ?=))))))

(defun pel-erlang-skel-filename ()
  "Insert name of current file."
  (concat "%%% File      : "
          (pel-current-buffer-filename :sans-directory)
          "\n"))

(defvar pel-skel-large-header
  '(o (pel-erlang-skel-optional-separator)
      (pel-erlang-skel-filename)
      (pel-skel-purpose-for (pel-erlang-skel-prompt-for-file-purpose-p)
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
      (pel-erlang-skel-file-doc)
      (pel-skel-include erlang-skel-small-header) )
  "*The template of a large header.
Please see the function `tempo-define-template'.")

;; -----------------------------------------------------------------------------
;; Tempo skeletons for OTP behaviours

(defun pel-erlang-skel-behaviour (behaviour api-funs callback-funs &optional exmpl-callback-funs)
  "Return a BEHAVIOUR description string with API-FUNS and CALLBACK-FUNS.
If EXMP-CALLBACK-FUNS is declared, its a list of function names that
act as exemples for names the user must create. "
  (concat
   (format "-behaviour(%s).\n\n" behaviour)
   (format "%%%% %s API\n"
           (pel-current-buffer-filename :sans-directory :sans-extension))
   (format "-export([%s]).\n\n" (pel-join api-funs
                                          ", "
                                          4 "         "))
   (format "%%%% %s callbacks\n" behaviour)
   (format "-export([%s]).\n" (pel-join callback-funs
                                          ", "
                                          4 "         "))
   (if exmpl-callback-funs
       (format "-export([%s]). %% Example functions: TODO: replace those with your names.\n\n"
               (pel-join exmpl-callback-funs
                         ", "
                         4 "         "))
     "\n")
   "-define(SERVER, ?MODULE).\n\n"))

(defun pel-erlang-skel-major-block (text &optional with-modname with-end-line)
  "Return a list of items to insert TEXT for the behaviour API block.
If WITH-MODNAME is non-nil the name of module is shown, prefixed to TEXT.
If WITH-END_LINE is non-nil, a terminating separator line is used."
  (let ((complete-text (format "%s%s"
                               (if with-modname
                                   (concat
                                    (pel-current-buffer-filename
                                     :sans-directory :sans-extension)
                                    " ")
                                 "")
                               text)))
    (list 'l
          (pel-erlang-skel-separator-start 3 ?=)
          "%%% "complete-text 'n
          (if pel-erlang-skel-use-secondary-separators
              ""
            (format "%%%%%% %s\n"
                    (make-string (length complete-text) ?=)))
          (pel-erlang-skel-optional-separator 3 ?=) 'n 'n
          (if with-end-line
              (pel-erlang-skel-separator)
            ""))))

(defun pel-erlang-skel-function-doc (title &optional description-lines is-private)
  "Return a tempo skel list for a private function ready for insertion.
The TITLE string is the function purpose.
The DESCRIPTION-LINES is a list of strings describing the function.
Identify private functions with non-nil IS-PRIVATE.
This puts Edoc annotations if Edoc is required by customization."
  (let ((sk (list 'l)))
    (pel-append-to sk (list (pel-erlang-skel-separator-start 2)))
    (if (pel-erlang-skel-edoc-in-function-p)
        (progn
          (when is-private
            (pel-append-to sk '("%% @private" n)))
          (pel-append-to sk (list (concat "%% @doc " title) 'n))
          (when description-lines
            (dolist (line description-lines)
              (pel-append-to sk (list (concat "%%      " line) 'n)))
            (pel-append-to sk (list "%% @end" 'n))))
      (if is-private
          (pel-append-to sk (list (concat "%% Private function: " title) 'n))
        (pel-append-to sk (list (concat "%% Function: " title) 'n)))
      (when description-lines
        (pel-append-to sk (list "%%" 'n))
        (dolist (line description-lines)
          (pel-append-to sk (list (concat "%% " line) 'n)))))
    (pel-append-to sk (list (pel-erlang-skel-optional-separator 2) 'n))
    sk))

;; Behaviour templates.
(defvar pel-erlang-skel-application
  '((pel-skel-include pel-skel-large-header)
    "-behaviour(application)." n n
    "%% Application callbacks" n
    "-export([start/2, start_phase/3, stop/1, prep_stop/1," n>
    "config_change/3])." n n
    (pel-erlang-skel-major-block "Application callbacks" :with-modname)
    (pel-erlang-skel-function-doc
     "Start application"
     '("Called when an application is started using application:start/[1,2],"
       "and application processes should be started."
       "If the application is structured according to the OTP design"
       "principles as a supervision tree, this means starting the"
       "top supervisor of the tree.")
     :is-private)
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
    (pel-erlang-skel-function-doc
     "Top supervisor of the tree."
     '("Starts an application with included applications, when"
       "synchronization is needed between processes in the different"
       "applications during startup.")
     :is-private)
    "-spec start_phase(Phase :: atom()," n>
    "StartType :: normal |" n>
    "{takeover, Node :: node()} |" n>
    "{failover, Node :: node()}," n>
    "PhaseArgs :: term()) -> ok | {error, Reason :: term()}." n
    "start_phase(_Phase, _StartType, _PhaseArgs) ->" n>
    "ok."n
    n
    (pel-erlang-skel-function-doc
     "Handle application stop request."
     '("Called whenever an application has stopped."
       "It is intended to be the opposite of Module:start/2 and should do"
       "any necessary cleaning up. The return value is ignored.")
     :is-private)
    "-spec stop(State :: term()) -> any()." n
    "stop(_State) ->" n>
    "ok." n
    n
    (pel-erlang-skel-function-doc
     "Handle preparation of application stop."
     '("Called when an application is about to be stopped,"
       "before shutting down the processes of the application.")
     :is-private)
    "-spec prep_stop(State :: term()) -> NewState :: term()." n
    "prep_stop(State) ->" n>
    "State." n
    n
    (pel-erlang-skel-function-doc
     "Handle code replacement configuration."
     '("Called by an application after a code replacement,"
       "if the configuration parameters have changed.")
     :is-private)
    "-spec config_change(Changed :: [{Par :: atom(), Val :: term()}]," n>
    "New :: [{Par :: atom(), Val :: term()}]," n>
    "Removed :: [Par :: atom()]) -> ok." n
    "config_change(_Changed, _New, _Removed) ->" n>
    "ok." n
    n
    (pel-erlang-skel-major-block "Internal functions" :with-modname :at-end))
  "Application behaviour tempo skeleton template.
Please see the function `tempo-define-template'.")

(defvar pel-erlang-skel-generic-server
  '((pel-skel-include pel-skel-large-header)
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
    (pel-erlang-skel-major-block "API" :with-modname)
    (pel-erlang-skel-function-doc "Start the server.")
    "-spec start_link() -> {ok, Pid :: pid()} |" n>
    "{error, Error :: {already_started, pid()}} |" n>
    "{error, Error :: term()} |" n>
    "ignore." n
    "start_link() ->" n>
    "gen_server:start_link({local, ?SERVER}, ?MODULE, [], [])." n
    n
    (pel-erlang-skel-major-block "gen_server callbacks")
    (pel-erlang-skel-function-doc "Initialize the server." nil :is-private)
    "-spec init(Args :: term()) -> {ok, State :: term()} |" n>
    "{ok, State :: term(), Timeout :: timeout()} |" n>
    "{ok, State :: term(), hibernate} |" n>
    "{stop, Reason :: term()} |" n>
    "ignore." n
    "init([]) ->" n>
    "process_flag(trap_exit, true)," n>
    "{ok, #state{}}." n
    n
    (pel-erlang-skel-function-doc "Handle call messages." nil :is-private)
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
    (pel-erlang-skel-function-doc "Handle cast messages." nil :is-private)
    "-spec handle_cast(Request :: term(), State :: term()) ->" n>
    "{noreply, NewState :: term()} |" n>
    "{noreply, NewState :: term(), Timeout :: timeout()} |" n>
    "{noreply, NewState :: term(), hibernate} |" n>
    "{stop, Reason :: term(), NewState :: term()}." n
    "handle_cast(_Request, State) ->" n>
    "{noreply, State}." n
    n
    (pel-erlang-skel-function-doc "Handle all non call/cast messages." nil :is-private)
    "-spec handle_info(Info :: timeout() | term(), State :: term()) ->" n>
    "{noreply, NewState :: term()} |" n>
    "{noreply, NewState :: term(), Timeout :: timeout()} |" n>
    "{noreply, NewState :: term(), hibernate} |" n>
    "{stop, Reason :: normal | term(), NewState :: term()}." n
    "handle_info(_Info, State) ->" n>
    "{noreply, State}." n
    n
    (pel-erlang-skel-function-doc
     "Terminate."
     '("Called by a gen_server when it is about to terminate."
       "It should be the opposite of Module:init/1 and do any"
       "necessary cleaning up."
       "When it returns, the gen_server terminates with Reason."
       "The return value is ignored.")
     :is-private)
    "-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term()," n>
    "State :: term()) -> any()." n
    "terminate(_Reason, _State) ->" n>
    "ok." n
    n
    (pel-erlang-skel-function-doc "Convert process state when code is changed."
                                  nil :is-private)
    "-spec code_change(OldVsn :: term() | {down, term()}," n>
    "State :: term()," n>
    "Extra :: term()) -> {ok, NewState :: term()} |" n>
    "{error, Reason :: term()}." n
    "code_change(_OldVsn, State, _Extra) ->" n>
    "{ok, State}." n
    n
    (pel-erlang-skel-function-doc
     "Handle status change request."
     '("Called for changing the form and appearance of gen_server"
       "server status when it is returned from sys:get_status/1,2"
       "or when it appears in termination error logs.")
     :is-private)
    "-spec format_status(Opt :: normal | terminate," n>
    "Status :: list()) -> Status :: term()." n
    "format_status(_Opt, Status) ->" n>
    "Status." n
    n
    (pel-erlang-skel-major-block "Internal functions" :with-modname :at-end))
  "Generic server tempo skeleton template.
Please see the function `tempo-define-template'.")

(defvar pel-erlang-skel-supervisor
  '((pel-skel-include pel-skel-large-header)
    (pel-erlang-skel-behaviour "supervisor"
                               '("start_link/0")
                               '("init/1"))
    (pel-erlang-skel-major-block "API Functions" :with-modname)
    (pel-erlang-skel-function-doc "Start the supervisor.")
    "-spec start_link() -> {ok, Pid :: pid()} |" n>
    "{error, {already_started, Pid :: pid()}} |" n>
    "{error, {shutdown, term()}} |" n>
    "{error, term()} |" n>
    "ignore." n
    "start_link() ->" n>
    "supervisor:start_link({local, ?SERVER}, ?MODULE, [])." n
    n
    (pel-erlang-skel-major-block "Supervisor callbacks")
    (pel-erlang-skel-function-doc
     "Initialize the server."
     '("Called by the new process when a supervisor is started"
       "using supervisor:start_link/[2,3] to find out about"
       "restart strategy, maximum restart intensity, and child"
       "specifications.")
     :is-private)
    "-spec init(Args :: term()) ->" n>
    "{ok, {SupFlags :: supervisor:sup_flags()," n>
    "[ChildSpec :: supervisor:child_spec()]}} |" n>
    "ignore." n
    "init([]) ->" n>
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
    (pel-erlang-skel-major-block "Internal functions" :with-modname :at-end))
  "Supervisor behaviour tempo skeleton template.
Please see the function `tempo-define-template'.")

(defvar pel-erlang-skel-supervisor-bridge
  '((pel-skel-include pel-skel-large-header)
    (pel-erlang-skel-behaviour "supervisor_bridge"
                               '("start_link/0")
                               '("init/1" "terminate/2"))
    "-record(state, {})." n n
    (pel-erlang-skel-major-block "API" :with-modname)
    (pel-erlang-skel-function-doc "Start the supervisor bridge.")
    "-spec start_link() -> {ok, Pid :: pid()} |" n>
    "{error, {already_started, Pid :: pid()}} |" n>
    "{error, term()} |" n>
    "ignore." n
    "start_link() ->" n>
    "supervisor_bridge:start_link({local, ?SERVER}, ?MODULE, [])." n
    n
    (pel-erlang-skel-major-block "supervisor_bridge callbacks")
    (pel-erlang-skel-function-doc
     "Initialize the supervisor bridge."
     '("Create a supervisor_bridge process, linked to the calling process,"
       "which calls Module:init/1 to start the subsystem. To ensure a"
       "synchronized start-up procedure, this function does not return"
       "until Module:init/1 has returned.")
     :is-private)
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
    (pel-erlang-skel-function-doc
     "Terminate."
     '("Called by the supervisor_bridge when it is about to terminate."
       "It should be the opposite of Module:init/1 and stop"
       "the subsystem and do any necessary cleaning up."
       "The return value is ignored.")
     :is-private)
    "-spec terminate(Reason :: shutdown | term(), State :: term()) -> any()." n
    "terminate(_Reason, _State) ->" n>
    "'AModule':stop()," n>
    "ok." n
    n
    (pel-erlang-skel-major-block "Internal functions" :with-modname :at-end))
  "Supervisor_Bridge behaviour tempo skeleton template.
Please see the function `tempo-define-template'.")

(defvar pel-erlang-skel-gen-event
  '((pel-skel-include pel-skel-large-header)
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
    (pel-erlang-skel-major-block "API" :with-modname)
    (pel-erlang-skel-function-doc "Create an event manager.")
    "-spec start_link() -> {ok, Pid :: pid()} |" n>
    "{error, Error :: {already_started, pid()} | term()}." n
    "start_link() ->" n>
    "gen_event:start_link({local, ?SERVER})." n
    n
    (pel-erlang-skel-function-doc "Add an event handler.")
    "-spec add_handler() -> ok | {'EXIT', Reason :: term()} | term()." n
    "add_handler() ->" n>
    "gen_event:add_handler(?SERVER, ?MODULE, [])." n
    n
    (pel-erlang-skel-major-block "gen_event callbacks")
    (pel-erlang-skel-function-doc
     "Initialize the event handler."
     '("Whenever a new event handler is added to an event manager,"
       "this function is called to initialize the event handler.")
     :is-private)
    "-spec init(Args :: term() | {Args :: term(), Term :: term()}) ->" n>
    "{ok, State :: term()} |" n>
    "{ok, State :: term(), hibernate} |" n>
    "{error, Reason :: term()}." n
    "init([]) ->" n>
    "{ok, #state{}}." n
    n
    (pel-erlang-skel-function-doc
     "Handle event."
     '("Called whenever an event manager receives an event sent using"
       "gen_event:notify/2 or gen_event:sync_notify/2, it is"
       "called for each installed event handler to handle the event.")
     :is-private)
    "-spec handle_event(Event :: term(), State :: term()) ->" n>
    "{ok, NewState :: term()} |" n>
    "{ok, NewState :: term(), hibernate} |" n>
    "remove_handler |" n>
    "{swap_handler, Args1 :: term(), NewState :: term()," n>
    "Handler2 :: atom() | {atom(), term()} , Args2 :: term()}." n>
    "handle_event(_Event, State) ->" n>
    "{ok, State}." n
    n
    (pel-erlang-skel-function-doc
     "Handle request."
     '("Called when an event manager receives a request sent using"
       "gen_event:call/3,4. It's called  for the specified"
       "event handler to handle the request.")
     :is-private)
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
    (pel-erlang-skel-function-doc
     "Handle info."
     '("Called for each installed event handler when"
       "an event manager receives any other message than an event or a"
       "synchronous request (or a system message).")
     :is-private)
    "-spec handle_info(Info :: term(), State :: term()) ->" n>
    "{ok, NewState :: term()} |" n>
    "{ok, NewState :: term(), hibernate} |" n>
    "remove_handler |" n>
    "{swap_handler, Args1 :: term(), NewState :: term()," n>
    "Handler2 :: atom() | {atom(), term()}, Args2 :: term()}." n
    "handle_info(_Info, State) ->" n>
    "{ok, State}." n
    n
    (pel-erlang-skel-function-doc
     "Terminate."
     '("Called when an event handler is deleted from an event manager."
       "It should be the opposite of Module:init/1 and"
       "do any necessary cleaning up.")
     :is-private)
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
    (pel-erlang-skel-function-doc
     "Convert process state when code is changed."
     nil
     :is-private)
    "-spec code_change(OldVsn :: term() | {down, term()}," n>
    "State :: term()," n>
    "Extra :: term()) -> {ok, NewState :: term()}." n
    "code_change(_OldVsn, State, _Extra) ->" n>
    "{ok, State}." n
    n
    (pel-erlang-skel-function-doc
     "Handle status change request."
     '("Called for changing the form and appearance"
       "of gen_event status when it is returned from sys:get_status/1,2"
       "or when it appears in termination error logs.")
     :is-private)
    "-spec format_status(Opt :: normal | terminate," n>
    "Status :: list()) -> Status :: term()." n
    "format_status(_Opt, Status) ->" n>
    "Status." n
    n
    (pel-erlang-skel-major-block "Internal functions" :with-modname :at-end))
  "Gen_event tempo skeleton template.
Please see the function `tempo-define-template'.")

(defvar pel-erlang-skel-gen-fsm
  '((erlang-skel-include pel-skel-large-header)
    (pel-erlang-skel-behaviour "gen_fsm"
                               '("start_link/0")
                               '("init/1"
                                 "handle_event/3"
                                 "handle_sync_event/4"
                                 "handle_info/3"
                                 "terminate/3"
                                 "code_change/4")
                               '("state_name/2"
                                 "state_name/3"))
    "-record(state, {})." n n
    (pel-erlang-skel-major-block "API" :with-modname)
    (pel-erlang-skel-function-doc
     "Start and link."
     '("Create a gen_fsm process which calls Module:init/1 to"
       "initialize. To ensure a synchronized start-up procedure, this"
       "function does not return until Module:init/1 has returned."))
    "-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Error :: term()}." n
    "start_link() ->" n>
    "gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], [])." n
    n
    (pel-erlang-skel-major-block "gen_fsm callbacks")
    (pel-erlang-skel-function-doc
     "Initialize."
     '("Whenever a gen_fsm is started using gen_fsm:start/[3,4] or"
       "gen_fsm:start_link/[3,4], this function is called by the new"
       "process to initialize.")
     :is-private)
    "-spec init(Args :: term()) -> {ok, StateName :: atom(), State :: term()} |" n>
    "{ok, StateName :: atom(), State :: term(), Timeout :: timeout()} |" n>
    "ignore |" n>
    "{stop, StopReason :: term()}." n
    "init([]) ->" n>
    "process_flag(trap_exit, true)," n>
    "{ok, state_name, #state{}}." n
    n
    (pel-erlang-skel-function-doc
     "Handle event at state."
     '("Whenever a gen_fsm receives an event sent using"
       "gen_fsm:send_event/2, the instance of this function with the same"
       "name as the current state name StateName is called to handle"
       "the event. It is also called if a timeout occurs."
       "TODO: Implement one such function for each state to handle,"
       "      replacing 'name' with the state name."
       "      There should be one function like this for each state name.")
     :is-private)
    "-spec state_name(Event :: term(), State :: term()) ->" n>
    "{next_state, NextStateName :: atom(), NextState :: term()} |" n>
    "{next_state, NextStateName :: atom(), NextState :: term(), Timeout :: timeout()} |" n>
    "{stop, Reason :: term(), NewState :: term()}." n
    "state_name(_Event, State) ->" n>
    "{next_state, state_name, State}." n
    n
    (pel-erlang-skel-function-doc
     "Handle event at state."
     '("Whenever a gen_fsm receives an event sent using"
       "gen_fsm:sync_send_event/[2,3], the instance of this function with"
       "the same name as the current state name StateName is called to"
       "handle the event."
       "TODO: Implement one such function for each state to handle,"
       "      replacing 'name' with the state name."
       "      There should be one function like this for each state name.")
     :is-private)
    "-spec state_name(Event :: term(), From :: term(), State :: term()) ->" n>
    "{next_state, NextStateName :: atom(), NextState :: term()} |" n>
    "{next_state, NextStateName :: atom(), NextState :: term(), Timeout :: timeout()} |" n>
    "{reply, Reply :: term(), NextStateName :: atom(), NextState :: term()} |" n>
    "{reply, Reply :: term(), NextStateName :: atom(), NextState :: term(), Timeout :: timeout()} |" n>
    "{stop, Reason :: term(), NewState :: term()} |" n>
    "{stop, Reason :: term(), Reply :: term(), NewState :: term()}." n
    "state_name(_Event, _From, State) ->" n>
    "Reply = ok," n>
    "{reply, Reply, state_name, State}." n
    n
    (pel-erlang-skel-function-doc
     "Handle event."
     '("Whenever a gen_fsm receives an event sent using"
       "gen_fsm:send_all_state_event/2, this function is called to handle"
       "the event.")
     :is-private)
    "-spec handle_event(Event :: term(), StateName :: atom(), State :: term()) ->" n>
    "{next_state, NextStateName :: atom(), NextState :: term()} |" n>
    "{next_state, NextStateName :: atom(), NextState :: term(), Timeout :: timeout()} |" n>
    "{stop, Reason :: term(), NewState ::term()}." n
    "handle_event(_Event, StateName, State) ->" n>
    "{next_state, StateName, State}." n
    n
    (pel-erlang-skel-function-doc
     "Handle synchronous event."
     '("Whenever a gen_fsm receives an event sent using"
       "gen_fsm:sync_send_all_state_event/[2,3], this function is called"
       "to handle the event.")
     :is-private)
    "-spec handle_sync_event(Event :: term(), From :: term(), StateName :: atom(), State :: term()) ->" n>
    "{next_state, NextStateName :: atom(), NextState :: term()} |" n>
    "{next_state, NextStateName :: atom(), NextState :: term(), Timeout :: timeout()} |" n>
    "{reply, Reply :: term(), NextStateName :: atom(), NextState :: term()} |" n>
    "{reply, Reply :: term(), NextStateName :: atom(), NextState :: term(), Timeout :: timeout()} |" n>
    "{stop, Reason :: term(), NewState :: term()} |" n>
    "{stop, Reason :: term(), Reply :: term(), NewState :: term()}." n
    "handle_sync_event(_Event, _From, StateName, State) ->" n>
    "Reply = ok," n>
    "{reply, Reply, StateName, State}." n
    n
    (pel-erlang-skel-function-doc
     "Handle info."
     '("This function is called by a gen_fsm when it receives any"
       "message other than a synchronous or asynchronous event"
       "(or a system message).")
     :is-private)
    "-spec handle_info(Info :: term(), StateName :: atom(), State :: term())->" n>
    "{next_state, NextStateName :: atom(), NextState ::term()} |" n>
    "{next_state, NextStateName :: atom(), NextState :: term(), Timeout :: timeout()} |" n>
    "{stop, Reason :: term(), NewState :: term()}." n
    "handle_info(_Info, StateName, State) ->" n>
    "{next_state, StateName, State}." n
    n
    (pel-erlang-skel-function-doc
     "Terminate."
     '("This function is called by a gen_fsm when it is about to"
       "terminate. It should be the opposite of Module:init/1 and do any"
       "necessary cleaning up. When it returns, the gen_fsm terminates with"
       "Reason. The return value is ignored.")
     :is-private)
    "-spec terminate(Reason :: term(), StateName :: atom(), State :: term()) -> OK." n
    "terminate(_Reason, _StateName, _State) ->" n>
    "ok." n
    n
    (pel-erlang-skel-function-doc
     "Handle code change."
     '("Convert process state when code is changed.")
     :is-private)
    "-spec code_change(OldVsn :: term(), StateName :: atom(), State :: term(), Extra :: term()) ->" n>
    "{ok, StateName :: atom(), NewState :: term()}." n
    "code_change(_OldVsn, StateName, State, _Extra) ->" n>
    "{ok, StateName, State}." n
    n
    (pel-erlang-skel-major-block "Internal functions" :with-modname :at-end))
  "Gen_fsm tempo skeleton template.
Please see the function `tempo-define-template'.")

(defvar pel-erlang-skel-gen-statem-StateName
  '((erlang-skel-include pel-skel-large-header)
    (pel-erlang-skel-behaviour "gen_statem"
                               '("start_link/0")
                               '("callback_mode/0"
                                 "init/1"
                                 "terminate/3"
                                 "code_change/4")
                               '("state_name/3"))
    "-record(data, {})." n n
    (pel-erlang-skel-major-block "API" :with-modname)
    (pel-erlang-skel-function-doc
     "Start and link."
     '("Create a gen_statem process which calls Module:init/1 to"
       "initialize. To ensure a synchronized start-up procedure, this"
       "function does not return until Module:init/1 has returned."))
    "-spec start_link() ->" n>
    "{ok, Pid :: pid()} |" n>
    "ignore |" n>
    "{error, Error :: term()}." n
    "start_link() ->" n>
    "gen_statem:start_link({local, ?SERVER}, ?MODULE, [], [])." n
    n
    (pel-erlang-skel-major-block "gen_statem callbacks")
    (pel-erlang-skel-function-doc
     "Set callback mode."
     '("Define the callback_mode() for this callback module.")
     :is-private)
    "-spec callback_mode() -> gen_statem:callback_mode_result()." n
    "callback_mode() -> state_functions." n
    n
    (pel-erlang-skel-function-doc
     "Initialize."
     '("Whenever a gen_statem is started using gen_statem:start/[3,4] or"
       "gen_statem:start_link/[3,4], this function is called by the new"
       "process to initialize.")
     :is-private)
    "-spec init(Args :: term()) ->" n>
    "gen_statem:init_result(atom())." n
    "init([]) ->" n>
    "process_flag(trap_exit, true)," n>
    "{ok, state_name, #data{}}." n
    n
    (pel-erlang-skel-function-doc
     "Handle state 'X'."
     '("Whenever a gen_statem receives an event, the function "
       "with the name of the current state (StateName) "
       "is called to handle the event."
       "TODO: Implement one such function for each event to handle,"
       "      replacing 'event' with the event name."
       "      There should be one function like this for each state name.")
     :is-private)
    "-spec state_name('enter'," n>
    "OldState :: atom()," n>
    "Data :: term()) ->" n>
    "gen_statem:state_enter_result('state_name');" n>
    "(gen_statem:event_type()," n>
    "Msg :: term()," n>
    "Data :: term()) ->" n>
    "gen_statem:event_handler_result(atom())." n
    ;;
    "state_name({call,Caller}, _Msg, Data) ->" n>
    "{next_state, state_name, Data, [{reply,Caller,ok}]}." n
    n
    (pel-erlang-skel-function-doc
     "Terminate."
     '("This function is called by a gen_statem when it is about to"
       "terminate. It should be the opposite of Module:init/1 and do any"
       "necessary cleaning up. When it returns, the gen_statem terminates with"
       "Reason. The return value is ignored.")
     :is-private)
    "-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->" n>
    "any()." n
    "terminate(_Reason, _State, _Data) ->" n>
    "void." n
    n
    (pel-erlang-skel-function-doc
     "Handle code change."
     '("Convert process state when code is changed")
     :is-private)
    "-spec code_change(" n>
    "OldVsn :: term() | {down,term()}," n>
    "State :: term(), Data :: term(), Extra :: term()) ->" n>
    "{ok, NewState :: term(), NewData :: term()} |" n>
    "(Reason :: term())." n
    "code_change(_OldVsn, State, Data, _Extra) ->" n>
    "{ok, State, Data}." n
    n
    (pel-erlang-skel-major-block "Internal functions" :with-modname :at-end))
  "Gen_statem (StateName/3) tempo skeleton template.
Please see the function `tempo-define-template'.")

(defvar pel-erlang-skel-gen-statem-handle-event
  '((erlang-skel-include pel-skel-large-header)
    (pel-erlang-skel-behaviour "gen_statem"
                               '("start_link/0")
                               '("callback_mode/0"
                                 "init/1"
                                 "terminate/3"
                                 "code_change/4")
                               '("handle_event/4"))
    "-record(data, {})." n n
    (pel-erlang-skel-major-block "API" :with-modname)
    (pel-erlang-skel-function-doc
     "Start and link."
     '("Create a gen_statem process which calls Module:init/1 to"
       "initialize. To ensure a synchronized start-up procedure, this"
       "function does not return until Module:init/1 has returned."))
    "-spec start_link() ->" n>
    "{ok, Pid :: pid()} |" n>
    "ignore |" n>
    "{error, Error :: term()}." n
    "start_link() ->" n>
    "gen_statem:start_link({local, ?SERVER}, ?MODULE, [], [])." n
    n
    (pel-erlang-skel-major-block "gen_statem callbacks")
    (pel-erlang-skel-function-doc
     "Set callback mode."
     '("Define the callback_mode() for this callback module.")
     :is-private)
    "-spec callback_mode() -> gen_statem:callback_mode_result()." n
    "callback_mode() -> handle_event_function." n
    n
    (pel-erlang-skel-function-doc
     "Initialize."
     '("Whenever a gen_statem is started using gen_statem:start/[3,4] or"
       "gen_statem:start_link/[3,4], this function is called by the new"
       "process to initialize.")
     :is-private)
    "-spec init(Args :: term()) ->" n>
    "gen_statem:init_result(term())." n
    "init([]) ->" n>
    "process_flag(trap_exit, true)," n>
    "{ok, state_name, #data{}}." n
    n
    (pel-erlang-skel-function-doc
     "Handle event 'X'."
     '("This function is called for every event a gen_statem receives."
       "TODO: Implement one such function for each event to handle,"
       "      replacing 'event' with the event name.")
     :is-private)
    "-spec handle_event('enter'," n>
    "OldState :: term()," n>
    "State :: term()," n>
    "Data :: term()) ->" n>
    "gen_statem:state_enter_result(term());" n>
    "(gen_statem:event_type()," n>
    "Msg :: term()," n>
    "State :: term()," n>
    "Data :: term()) ->" n>
    "gen_statem:event_handler_result(term())." n
    ;;
    "handle_event({call,From}, _Msg, State, Data) ->" n>
    "{next_state, State, Data, [{reply,From,ok}]}." n
    n
    (pel-erlang-skel-function-doc
     "Terminate."
     '("This function is called by a gen_statem when it is about to"
       "terminate. It should be the opposite of Module:init/1 and do any"
       "necessary cleaning up. When it returns, the gen_statem terminates with"
       "Reason. The return value is ignored.")
     :is-private)
    "-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->" n>
    "any()." n
    "terminate(_Reason, _State, _Data) ->" n>
    "void." n
    n
    (pel-erlang-skel-function-doc
     "Handle code change."
     '("Convert process state when code is changed")
     :is-private)
    "-spec code_change(" n>
    "OldVsn :: term() | {down,term()}," n>
    "State :: term(), Data :: term(), Extra :: term()) ->" n>
    "{ok, NewState :: term(), NewData :: term()} |" n>
    "(Reason :: term())." n
    "code_change(_OldVsn, State, Data, _Extra) ->" n>
    "{ok, State, Data}." n
    n
    (pel-erlang-skel-major-block "Internal functions" :with-modname :at-end))
  "Gen_statem (handle_event/4) tempo skeleton template.
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
  "Key mapping for skeletons defined in erlang-skel.el.
Each element of the list has one of the 2 following forms:
- a 2 element (name . key) cons cell,
- a 3 element (name key preliminary-function) list.
The first element is always the template name.
The second element is always a key sequence string.
The third element is optional.  If present, it is the
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
           (boundp 'erlang-skel-gen-statem-StateName)
           (boundp 'erlang-skel-gen-statem-handle-event)
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
      (setq erlang-skel-gen-statem-StateName
            pel-erlang-skel-gen-statem-StateName)
      (setq erlang-skel-gen-statem-handle-event
            pel-erlang-skel-gen-statem-handle-event)
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
