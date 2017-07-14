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

## Rest API Reference

Not yet.
