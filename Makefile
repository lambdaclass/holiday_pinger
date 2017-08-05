.PHONY: dev test release ops ops_start

dev:
	./rebar3 compile && ./rebar3 shell --config conf/dev.config
test:
	./rebar3 ct
release:
	./rebar3 as prod release tar
ops:
	docker-compose -f docker/docker-compose.yml up

ops_start:
	docker run -it --link docker_postgres_1:postgres --net docker_default -v ${shell pwd}/priv/sql/tables.sql:/data/tables.sql --env PGPASSWORD=example --rm postgres /bin/bash -c "createdb -h docker_postgres_1 -U postgres holiday_ping ; psql -h docker_postgres_1 -U postgres -a -f /data/tables.sql"

dev_ui:
	test -d _ui || git clone git@github.com:lambdaclass/holiday_ping_ui.git _ui && \
	rm -rf priv/ui && ln -s ${shell pwd}/_ui/resources/public/ ${shell pwd}/priv/ui && \
	cd _ui && ${shell command -v rlwrap} lein figwheel

release_ui:
	test -d _ui || git clone git@github.com:lambdaclass/holiday_ping_ui.git _ui && \
	cd _ui && lein do clean, cljsbuild once min && cd .. && \
	rm -rf priv/ui && cp -r _ui/resources/public/ priv/ui
