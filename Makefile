compile:
	rebar compile skip_deps=true

clean:
	rebar clean skip_deps=true
	rm -f erl_crash.dump

eunit: compile
	rebar eunit skip_deps=true

run:
	erl +pc unicode -pa ebin

d:
	dialyzer -I include --src src test \
	| fgrep --invert-match --file .dialyzer.ignore

etags:
	etags src/*
