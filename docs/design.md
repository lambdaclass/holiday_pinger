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

### Om vs Reagent vs Om.next vs re-frame

Reagent seems more simple and idiomatic (data oriented), om uses
protocols, reify, functions instead of data to represent the dom.
Om apparently requires more investment up front, and can be better for big
applicationsd (which is not our case).

https://news.ycombinator.com/item?id=13379609
https://www.reddit.com/r/Clojure/comments/4i16v0/is_there_a_better_library_than_reframe_in_its/
https://www.reddit.com/r/Clojure/comments/3vk58p/a_rant_on_om_next/
https://www.reddit.com/r/Clojure/comments/421gqj/why_we_use_om_and_why_were_excited_for_om_next/

reagent seems very clean and re-frame too, but the latter seems to have a preference
for global, implicit state in it's api that I'm not too thrilled about:
you call reg-event-db and that register your handler somewhere, instead of
you explicitly passing it to some initialization function. As a consequence
you need to require namespaces that you aren't actually using, just so those
registrations take place

some reptitive tasks: handlers that just set a value, subs that just read a value from db
looks like an interesting structure/pattern to guarantee decoupling, but at times
feels like too much for a small project.
