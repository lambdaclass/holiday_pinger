# Design decisions

## Erlang vs Elixir

why we prefer to learn elrang/show erlang skills before elixir.

## Supervisor structure

one simple one for one to start.
possibly another simple one for one to isolate different channels
why not gen event
why not pools

## API
### identifiers
db id vs names vs uuid

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
https://purelyfunctional.tv/article/why-re-frame-instead-of-om-next/


reagent seems very clean and re-frame too, but the latter seems to have a preference
for global, implicit state in it's api that I'm not too thrilled about:
you call reg-event-db and that register your handler somewhere, instead of
you explicitly passing it to some initialization function. As a consequence
you need to require namespaces that you aren't actually using, just so those
registrations take place

some reptitive tasks: handlers that just set a value, subs that just read a value from db
looks like an interesting structure/pattern to guarantee decoupling, but at times
feels like too much for a small project.

quote by re-frame author:

```
mikethompson [3:34 AM]
@sandbags @gklijs I always have the same response :-)  You can absolutely use Reagent by itself if your application is simple enough.  BUT if  you just use Reagent by itself then you only have the V bit of an application.  As your application starts to get more complicated, you **will** start to create an architecture which adds `control logic` and `state management` - the M and C parts (even if you don't think you are, you are).  So then the question becomes:  is your architecture better than re-frame's or not?  And some people prefer their own architectures and would answer "yes" :-)   Fair enough.
I think the only danger arises if this process is not conscious - if someone creates a dogs breakfast of an architecture and doesn;t even know they've done it.  I've had MANY people privately admit that's what happened to them ... and then they swapped to re-frame to get some structure back.
So, my advice is .... if your application is a little more complicated, be sure to make a conscious choice around architecture, because one way or another you'll be using one.
```

for a strongly structured and opinionated framework, it feels lacking
a standarad for routing and handling the data loading associated with a
each route. It's a tricky thing to implement on a per app basis.

## Random notes

- Sometimes you need the UI in order to better understand the model. You need to be able
to poke around with the app to see what feels right and what not, and maybe change
the model accordingly. Having your db schema carved on stone from the start of the project sucks.
The same can be said about a models file or your REST URIs.

Once we saw the UI, we knew we have to turn around the relations between our
entities, and what's more when we did the code turned out to be simpler
(a supervisor and gen server we had recently introduced became unnecessary)

The same happened with country detection: after realizing channel creation demanded a wizard
ui, country selection was an obvious candidate for a wizard step, thus removing the need to
depend on an external source for country detection, dramatically simplifying github login flow, etc.

- This is nothing new, but frontend takes way more time than backend.

- although it's super fun to code a frontend app with cljs, it's great
to get some experience with react/re-frame, and get in touch with fronted develpment
again, I have to say I agree on what a bad idea it is to make everything a
SPA. Apps that don't inherently have a lot of dynamic components (e.g. google docs),
and instead are more classic (navigation oriented) make you invest a ton of time
just reproducing the native behavior of the browser and you get little of the benefits
of using a js framework.
