# Holiday Ping [![Build Status](https://travis-ci.org/lambdaclass/holiday_ping.svg?branch=master)](https://travis-ci.org/lambdaclass/holiday_ping)

Holiday ping is an Erlang/OTP application that allows users to send national
holiday reminders through different channels (e.g. email, Slack).

## Project setup for development

The project requires Elrang/OTP 20 and Leningen for the ClojureScript web application.

Setup the database (using Docker):

    $ make ops

Create the tables:

    $ make ops_start

Compile and run a development shell:

    $ make dev

Run the tests:

    $ make test

Build a release:

    $ make release

### UI setup

The UI is a ClojureScript project that uses [re-frame](https://github.com/Day8/re-frame).
The code is in [priv/ui](https://github.com/lambdaclass/holiday_ping/tree/master/priv/ui) and is build as part of the `dev` and `release` targets.

For UI development, run a dev shell and then:

    $ make dev_ui

Which uses figwheel to provide a REPL and hot-reload of the code changes.

### GitHub login configuration

For the GitHub login option to work, [OAuth cretentials](https://github.com/settings/applications/new)
need to be generated and set as `GITHUB_CLIENTID` and `GITHUB_SECRET`.

## Rest API Reference

Not yet.
