%%%-------------------------------------------------------------------
%%% Part of rebar3_warn_outdated_deps Erlang App
%%% MIT License
%%% Copyright (c) 2021 Jose Maria Perez Ramos
%%%-------------------------------------------------------------------
-module(rebar3_warn_outdated_deps_common).

%% API
-export([report/1]).


%%====================================================================
%% API functions
%%====================================================================

-type config_dep() :: atom() | tuple().
-spec report(rebar_state:t()) -> ReportedDeps :: [{Profile :: atom(), config_dep()}].
report(State) ->
    ConfigDeps = config_deps(State),

    % Organize deps in both
    % #{profile ->  set(config_dep)}
    ConfigDepsByProfile = lists:foldl(fun({Profile, ConfigDep}, Acc) ->
                                              Acc#{Profile => (maps:get(Profile, Acc, #{}))#{ConfigDep => true}}
                                      end, #{}, ConfigDeps),
    % #{DepName :: binary() -> set(profile)}
    ConfigProfilesByDepName = lists:foldl(fun({Profile, ConfigDep}, Acc) ->
                                                  DepName = if is_atom(ConfigDep) -> atom_to_binary(ConfigDep, utf8);
                                                               true -> atom_to_binary(element(1, ConfigDep), utf8)
                                                            end,
                                                  Acc#{DepName => (maps:get(DepName, Acc, #{}))#{Profile => true}}
                                          end, #{}, ConfigDeps),

    % Report all top level deps that are not in the config
    % these are only reported
    lists:foreach(fun(Dep) ->
                          DepName = rebar_app_info:name(Dep),
                          IsConfigured = maps:is_key(DepName, ConfigProfilesByDepName),
                          case {rebar_app_info:dep_level(Dep), IsConfigured, rebar_app_info:is_lock(Dep)} of
                              {0, false, true} -> rebar_api:warn("~ts locked, missing in rebar.config", [DepName]);
                              {0, false, false} -> rebar_api:warn("~ts present, missing in rebar.config", [DepName]);
                              _ -> ok
                          end
                  end, rebar_state:all_deps(State)),

    % Iterate over the profiles, parsing the deps without involving locks and
    % checking if the dependency should be reported. Returns a list of the
    % reported ones. Reports at most once each dep.
    lists:foldl(fun(Profile, Acc) ->
                        DepsDir = rebar_prv_install_deps:profile_dep_dir(State, Profile),
                        ProfileDeps = maps:keys(maps:get(Profile, ConfigDepsByProfile, #{})),
                        ProfileNewDeps = ProfileDeps -- [ConfigDep || {_, ConfigDep} <- Acc],
                        ParsedDeps = rebar_app_utils:parse_deps(root, DepsDir, ProfileNewDeps, State, [], 0),
                        lists:foldl(fun({ConfigDep, ParsedDep}, InnerAcc) ->
                                            DepWithSource = rebar_app_utils:expand_deps_sources(ParsedDep, State),
                                            case dep_is_reported(Profile, DepWithSource, State) of
                                                true -> [{Profile, ConfigDep} | InnerAcc];
                                                false -> InnerAcc
                                            end
                                    end, Acc, lists:zip(ProfileNewDeps, ParsedDeps))
                end, [], rebar_state:current_profiles(State)).


-spec dep_is_reported(atom(), rebar_app_info:t(), rebar_state:t()) -> boolean().
dep_is_reported(Profile, Dep, State) ->
    % Reporting checkouts, unlocked and apps requiring updates
    DepName = rebar_app_info:name(Dep),
    case rebar_app_info:source(Dep) of
        checkout ->
            rebar_api:warn("Profile ~p: ~ts is a checkout", [Profile, DepName]),
            true;
        _ ->
            case rebar_fetch:needs_update(Dep, State) of
                true ->
                    rebar_api:warn("Profile ~p: ~ts doesn't match rebar.config", [Profile, DepName]),
                    true;
                false when Profile == default -> % Only check if dep is locked for the default profile
                    Locks = rebar_state:get(State, {locks, default}, []),
                    MissingLock = lists:keyfind(DepName, 1, Locks) == false,
                    if MissingLock -> rebar_api:warn("Profile ~p: ~ts is not locked", [Profile, DepName]);
                       true -> ok
                    end,
                    MissingLock;
                false ->
                    false
            end
    end.


-spec config_deps(rebar_state:t()) -> [{Profile :: atom(), config_dep()}].
config_deps(State) ->
    % Get configured deps with their profile
    CurrentProfiles = rebar_state:current_profiles(State),
    % root rebar.config deps tuple
    TopConfigDeps = [{default, D} || D <- rebar_state:get(State, deps, [])],
    % root rebar.config profiled (Profile /= default) and app-level rebar.config
    % (Profile == default)
    ProfiledConfigDeps = lists:append([[{Profile, D} || D <- rebar_state:get(State, {deps, Profile}, [])] || Profile <- CurrentProfiles]),
    % to avoid preprocessed deps, we take the ones with atoms only, see
    % rebar_prv_upgrade.erl
    lists:foldl(fun({Profile, ConfigDepName}, Acc) when is_atom(ConfigDepName) ->
                        [{Profile, ConfigDepName}| Acc];
                   ({Profile, ConfigDep}, Acc) when is_atom(element(1, ConfigDep)) ->
                        [{Profile, ConfigDep}| Acc];
                   (_Dep , Acc) ->
                        Acc
                end, [], TopConfigDeps ++ ProfiledConfigDeps).

