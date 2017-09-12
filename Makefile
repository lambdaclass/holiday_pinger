.PHONY: dev test release ops ops_start

dev:
	test -f priv/ui/resources/public/js/compiled/app.js || \
	(cd priv/ui && lein cljsbuild once dev && cd ../..) && \
	./rebar3 compile && ./rebar3 shell --config conf/dev.config

test:
	./rebar3 ct

release:
	cd priv/ui && lein do clean, cljsbuild once min && cd ../.. && \
	./rebar3 as prod release tar

ops:
	docker-compose -f docker/docker-compose.yml up

ops_reset:
	docker-compose -f docker/docker-compose.yml down

ops_start:
	docker run -it --link docker_postgres_1:postgres --net docker_default -v ${shell pwd}/priv/sql/tables.sql:/data/tables.sql --env PGPASSWORD=example --rm postgres /bin/bash -c "createdb -h docker_postgres_1 -U postgres holiday_ping ; psql -h docker_postgres_1 -U postgres -a -f /data/tables.sql"

dev_ui:
	cd priv/ui && ${shell command -v rlwrap} lein figwheel
