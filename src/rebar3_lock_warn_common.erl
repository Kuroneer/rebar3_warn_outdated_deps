% Part of rebar3_lock_warn Erlang App (rebar3 plugin)
% MIT License
% Copyright (c) 2019 Jose Maria Perez Ramos

-module(rebar3_lock_warn_common).

-export([do/2]).


%% ===================================================================
%% Public API
%% ===================================================================

-spec do(rebar_state:t(), boolean()) -> {ok, rebar_state:t()} | {error, string()}.
do(State, AbortOnMismatch) ->
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

    AnyWarned = maps:fold(
                  fun(DefinedDepName, DefinedDep, Warned) ->
                          DefinedDepSource = rebar_app_info:source(DefinedDep),
                          AllDepsDepSource = rebar_app_info:source(maps:get(DefinedDepName, AllDeps)),
                          case DefinedDepSource of
                              checkout ->
                                  rebar_api:warn("~ts is a checkout", [DefinedDepName]),
                                  true;
                              AllDepsDepSource ->
                                  Warned;
                              _ ->
                                  case rebar_fetch:needs_update(DefinedDep, State) of
                                      true ->
                                          rebar_api:warn("~ts differs from lock file", [DefinedDepName]),
                                          true;
                                      _ ->
                                          Warned
                                  end
                          end
                  end,
                  false,
                  DefinedDeps),

    case AnyWarned and AbortOnMismatch of
        true ->
            rebar_api:abort("Mismatch found between lock and config", []);
        _ ->
            ok
    end,

    {ok, State}.


%% ===================================================================
%% Private functions
%% ===================================================================

deplist_to_map(CurrentDepsMap, DepsList) ->
    lists:foldl(fun(Dep, DepsIn) -> DepsIn#{rebar_app_info:name(Dep) => Dep} end, CurrentDepsMap, DepsList).

