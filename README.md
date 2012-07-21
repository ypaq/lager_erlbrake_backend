Lager Backend for Airbrake
==========================

This is a [Lager][lager] backend for the [Airbrake][airbrake] error notification service. 

How to use?
-----------

Add `lager_erlbrake_backend` to the dependencies in your `rebar.config`:

    {lager_erlbrake_backend, ".*", {git, "https://github.com/ypaq/lager_erlbrake_backend.git", "master"}}

Include `lager_erlbrake_backend` in the `lager` configuration of your project:

    {lager, [handlers, [{lager_erlbrake_backend, []}] ] }


Build
-----

Build with: 

    ./rebar get-deps
    ./rebar compile


Dependencies
------------

Dependencies are listed here for completeness but are managed with `rebar`:

* [lager][lager] (lastest version after riak 1.2) 
* [erlbrake][erlbrake]


[lager]: <http://github.com/basho/lager> "Lager"
[erlbrake]: <http://github.com/kenpratt/erlbrake> "Erlbrake"
[airbrake]: <http://airbrake.io> "Airbrake"

