dev_shell:
	rebar3 compile && env ERL_FLAGS="-config conf/dev.config" rebar3 shell
