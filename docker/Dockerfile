########
FROM clojure:lein-2.5.3

WORKDIR /opt/holiday_ping/priv/ui

COPY priv/ui .
RUN lein deps
RUN lein do clean, cljsbuild once min

##########
FROM erlang:20.3.8.23-alpine

WORKDIR /opt/holiday_ping

RUN apk add --update bsd-compat-headers build-base git

COPY rebar.config rebar.lock ./
RUN rebar3 compile

COPY conf conf
COPY src src
COPY --from=0 /opt/holiday_ping/priv/ui/resources/public priv/ui/resources/public
RUN rebar3 compile
RUN rebar3 as prod release tar
RUN mkdir rel
RUN tar -vxf _build/prod/rel/holiday_ping/*.tar.gz -C rel

#######
FROM alpine:3.9

WORKDIR /opt/holiday_ping

RUN apk add --update ncurses

COPY --from=1 /opt/holiday_ping/rel .
# COPY --from=1 /opt/holiday_ping/priv/ui priv/ui
# COPY --from=1 /opt/holiday_ping/priv/ui /ui
# COPY --from=1 /opt/holiday_ping/priv/ui bin/priv/ui

ENTRYPOINT ["/opt/holiday_ping/bin/holiday_ping"]
CMD ["foreground"]
