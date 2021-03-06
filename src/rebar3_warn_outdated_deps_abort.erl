%%%-------------------------------------------------------------------
%%% Part of rebar3_warn_outdated_deps Erlang App
%%% MIT License
%%% Copyright (c) 2021 Jose Maria Perez Ramos
%%%-------------------------------------------------------------------
-module(rebar3_warn_outdated_deps_abort).

%% API
-export([
         init/1,
         do/1,
         format_error/1
        ]).

-define(PROVIDER, warn_outdated_deps_abort).
-define(DEPS, [lock]).


%%====================================================================
%% API functions
%%====================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 warn_outdated_deps_abort"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "Aborts when a locked dep needs to be updated to match rebar.config"},
            {desc,       "Aborts when a locked dep needs to be updated to match rebar.config",
             " Alias for 'warn_outdated_deps -a'"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    case rebar3_warn_outdated_deps_common:report(State) of
        [] -> {ok, State};
        _ -> rebar_api:abort("Mismatch found between local and config, abort", [])
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

