# Lager Backend for Airbrake


This is a [lager][lager] backend for the error notification service [Airbrake][airbrake]. 
The backend mediates between lager and [erlbrake][erlbrake]. 


## Usage

You need an [Airbrake][airbrake] account. Your application should be OTP conform and use `rebar`.

Add `lager_erlbrake_backend` to the dependencies in your `rebar.config`:

    {lager_erlbrake_backend, ".*", {git, "https://github.com/ypaq/lager_erlbrake_backend.git", "master"}}

Include `lager_erlbrake_backend` in the `lager` configuration of your project:

    {lager, [handlers, [{lager_erlbrake_backend, [ {environment, "development"},
                                                   {api_key, "ENTER_API_KEY"},
                                                   {notification_api, "http://airbrake.io/notifier_api/v2/notices"}, %% optional
                                                   {notify_level, error}  %% optional 
                                                 ]}]]}

The backend will send log messages with log level `error` or more critical to Airbrake.

Optional parameter notification_api is used to support an alternative Airbrake compliant
endpoint. For example, using "https://api.rollbar.com/notifier_api/v2/notices/" you can use
the compliant endpoint from Rollbar service.


### Build

You can build `lager_erlbrake_backend` on its own with: 

    ./rebar get-deps compile

### Dependencies

Dependencies are listed here for completeness, but are managed with `rebar`:

* [lager][lager] (version after riak 1.2) 
* [erlbrake][erlbrake]


[lager]: <http://github.com/basho/lager> "lager"
[erlbrake]: <http://github.com/kenpratt/erlbrake> "erlbrake"
[airbrake]: <http://airbrake.io> "Airbrake"

