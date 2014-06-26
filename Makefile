compile:
	rebar compile skip_deps=true

clean:
	rebar clean skip_deps=true
	rm -f erl_crash.dump

eunit:
	rebar eunit skip_deps=true

run:
	erl +pc unicode -pa ebin

d:
	dialyzer --src src test

etags:
	etags src/*
