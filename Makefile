REBARVER = 3.15.2
ifeq ($(OTPVER),24.0)
	REBARVER = 3.17.0
endif


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

rebar3:
	wget https://github.com/erlang/rebar3/releases/download/${REBARVER}/rebar3 &&\
	chmod u+x rebar3
