# Holiday Ping [![Build Status](https://travis-ci.org/lambdaclass/holiday_ping.svg?branch=master)](https://travis-ci.org/lambdaclass/holiday_ping)

Holiday ping is an Erlang/OTP application that allows users to send national
holiday reminders through different channels (e.g. email, Slack).

## Project setup for development

The project requires Elrang/OTP 20.

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

The UI code is maintained in the [holiday_ping_ui](https://github.com/lambdaclass/holiday_ping_ui) repository.

For UI development, run:

    $ make dev_ui

This will clone the ui project to the `_ui` folder, link the relevant files
to the priv folder of the erlang project and run figwheel for hot reload
of the changes in the UI code.

To build the ui and include it in the erlang project:

    $ make release_ui

## Rest API Reference

Not yet.
