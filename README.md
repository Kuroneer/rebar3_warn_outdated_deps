# rebar3_lock_warn

Rebar3 plugin that warns about mismatches between defined and locked dependencies.

## Build

    $ rebar3 compile

## Use

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_lock_warn, {git, "https://github.com/Kuroneer/rebar3_lock_warn.git", {tag, "0.1.0"}}}
    ]}.

Then just call it directly in an existing application:

    $ rebar3 lock_warn
    ===> Compiling rebar3_lock_warn
    ===> Verifying dependencies...
    ===> cowboy differs from lock file

    $ rebar3 lock_warn -a
    ===> Compiling rebar3_lock_warn
    ===> Verifying dependencies...
    ===> cowboy differs from lock file
    ===> cowboy mismatch found: Abort

or set up a hook:

    {provider_hooks, [{pre, [{compile, lock_warn}]}]}.

## Authors

* **Jose M Perez Ramos** - [Kuroneer](https://github.com/Kuroneer)

## License

[MIT License](LICENSE)

