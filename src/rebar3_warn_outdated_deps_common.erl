% Part of rebar3_warn_outdated_deps Erlang App (rebar3 plugin)
% MIT License
% Copyright (c) 2019 Jose Maria Perez Ramos

-module(rebar3_warn_outdated_deps_common).

-export([do/2]).


%%% API

-spec do(rebar_state:t(), boolean()) -> {ok, rebar_state:t()} | {error, string()}.
do(State, AbortOnMismatch) ->
    CurrentProfiles = rebar_state:current_profiles(State),

    DefinedDeps = lists:foldl(
                    fun(Profile, DefinedDepsIn) ->
                            DepsDir = rebar_prv_install_deps:profile_dep_dir(State, Profile),
                            ProfileDeps = rebar_state:get(State, {deps, Profile}, []),
                            ConfiguredDeps = lists:filter(
                                               fun({Name, Src, Level}) when is_tuple(Src), is_integer(Level) ->
                                                       rebar_api:warn("~ts locked, missing in rebar.config", [Name]),
                                                       false;
                                                  (_) ->
                                                       true
                                               end, ProfileDeps),
                            ParsedDeps = rebar_app_utils:parse_deps(root, DepsDir, ConfiguredDeps, State, [], 0),
                            lists:foldl(
                              fun(ParsedDep, DepsIn) ->
                                      DepWithSource = rebar_app_utils:expand_deps_sources(ParsedDep, State),
                                      DepsIn#{rebar_app_info:name(DepWithSource) => DepWithSource}
                              end, DefinedDepsIn, ParsedDeps)
                    end, #{}, CurrentProfiles),

    Warned = maps:filter(
               fun(DepName, Dep) ->
                       case rebar_app_info:source(Dep) of
                           checkout ->
                               rebar_api:warn("~ts is a checkout", [DepName]),
                               true;
                           _ ->
                               case rebar_fetch:needs_update(Dep, State) of
                                   true ->
                                       rebar_api:warn("~ts doesn't match rebar.config", [DepName]),
                                       true;
                                   _ ->
                                       false
                               end
                       end
               end, DefinedDeps),

    case AbortOnMismatch of
        true when map_size(Warned) > 0 ->
            rebar_api:abort("Mismatch found between local and config, abort", []);
        _ ->
            {ok, State}
    end.

