% Part of rebar3_warn_outdated_deps Erlang App (rebar3 plugin)
% MIT License
% Copyright (c) 2019 Jose Maria Perez Ramos

-module(rebar3_warn_outdated_deps_abort).

-export([
         init/1,
         do/1,
         format_error/1
        ]).

-define(PROVIDER, warn_outdated_deps_abort).
-define(DEPS, [lock]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 warn_outdated_deps_abort"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "Aborts when a dep needs to be updated to match rebar.config"},
            {desc,       "Aborts when a dep needs to be updated to match rebar.config",
             " Commodity command for 'warn_outdated_deps -a'"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar3_warn_outdated_deps_common:do(State, true).


-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

