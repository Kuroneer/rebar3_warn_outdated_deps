% Part of rebar3_lock_warn Erlang App (rebar3 plugin)
% MIT License
% Copyright (c) 2019 Jose Maria Perez Ramos

-module(rebar3_lock_warn).

-export([
         init/1,
         do/1,
         format_error/1
        ]).

-define(PROVIDER, lock_warn).
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
            {example, "rebar3 lock_warn"}, % How to use the plugin
            {opts, [                      % list of options understood by the plugin
                    {abort_on_mismatch, $a, "abort_on_mismatch", boolean, "Abort if a mismatch is found. Default: false"}
                   ]},
            {short_desc, "Warns when you have mismatching locked dependencies defined"},
            {desc, "Warns when you have mismatching locked dependencies defined"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    CurrentProfiles = rebar_state:current_profiles(State),
    DefinedDeps = lists:foldl(
                    fun(Profile, DefinedDepsIn) ->
                            ProfileDeps = rebar_state:get(State, {deps, Profile}, []),
                            DepsDir = rebar_prv_install_deps:profile_dep_dir(State, Profile),
                            ParsedDeps = rebar_app_utils:parse_deps(root, DepsDir, ProfileDeps, State, [], 0),
                            ParsedDepsWithSources = [rebar_app_utils:expand_deps_sources(Dep, State)
                                                     || Dep <- ParsedDeps],
                            deplist_to_map(DefinedDepsIn, ParsedDepsWithSources)
                    end, #{}, CurrentProfiles),

    AllDeps = deplist_to_map(#{}, rebar_state:all_deps(State)),

    {Args, _} = rebar_state:command_parsed_args(State),
    AbortOnMismatch = proplists:get_value(abort_on_mismatch, Args, false),

    maps:fold(
      fun(DefinedDepName, DefinedDep, _) ->
              DefinedDepSource = rebar_app_info:source(DefinedDep),
              AllDepsDepSource = rebar_app_info:source(maps:get(DefinedDepName, AllDeps)),
              case DefinedDepSource of
                  AllDepsDepSource ->
                      ok;
                  _ ->
                      case rebar_fetch:needs_update(DefinedDep, State) of
                          true ->
                              rebar_api:warn("~ts differs from lock file", [DefinedDepName]),
                              case AbortOnMismatch of
                                  true ->
                                      rebar_api:abort("~ts mismatch found: Abort", [DefinedDepName]);
                                  _ ->
                                      ok
                              end;
                          _ ->
                              ok
                      end
              end
      end,
      undefined,
      DefinedDeps),

    {ok, State}.


deplist_to_map(CurrentDepsMap, DepsList) ->
    lists:foldl(fun(Dep, DepsIn) -> DepsIn#{rebar_app_info:name(Dep) => Dep} end, CurrentDepsMap, DepsList).


-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

