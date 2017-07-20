# Design decisions

## Erlang vs Elixir

## Supervisor structure

one simple one for one to start.
possibly another simple one for one to isolate different channels
why not gen event
why not pools

## Database decisions

### Database Inheritance

Has limitations (doc link), constraints are lost and ends up being worse to manage
than plain tables.

### Plain tables

Way to go on the long run, beneficial constraints but more development overhead
in early stages than.

### hstore

doesn't have arrays.

### jsonb

Useful for development until supported channels are settled and we know what the
data looks like.

## Rest Auth: Basic, Tokens, jwt

## UI: JavaScript, ClojureScript, Elm?

### Om vs Reagent vs Om.next vs Reframe
