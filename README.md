# rebar3_warn_outdated_deps

Rebar3 plugin that warns when a dep needs to be locked or updated to match rebar.config

## Build

    $ rebar3 compile

## Use

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_warn_outdated_deps, {git, "https://github.com/Kuroneer/rebar3_warn_outdated_deps.git", {tag, "0.2.0"}}}
    ]}.

Then just call it directly in an existing application:

    $ rebar3 warn_outdated_deps_abort
    ===> Verifying dependencies...
    ===> cowboy doesn't match rebar.config
    ===> tcpserver is a checkout
    ===> Mismatch found between local and config, abort


or set up a hook (its intended use):

    {provider_hooks, [{pre, [{compile, warn_outdated_deps}]}]}.

or

    {provider_hooks, [{pre, [{compile, warn_outdated_deps_abort}]}]}.


## Authors

* **Jose M Perez Ramos** - [Kuroneer](https://github.com/Kuroneer)

## License

[MIT License](LICENSE)

