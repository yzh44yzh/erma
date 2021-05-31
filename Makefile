compile:
	rebar3 compile

clean:
	rebar3 clean
	rm -f erl_crash.dump

eunit: 
	rebar3 eunit

run:
	erl +pc unicode -pa ebin

d:
	dialyzer -I include --src src test \
	| fgrep --invert-match --file .dialyzer.ignore

etags:
	etags src/*
