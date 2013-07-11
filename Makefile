REBAR = `which rebar || ./rebar`

all: compile

compile:
	$(REBAR) get-deps
	$(REBAR) compile

clean:
	$(REBAR) clean

eunit:
	$(REBAR) eunit skip_deps=true

check:
	dialyzer -pa ebin deps/jsx/ebin

shell: compile
	erl -pa ebin deps/jsx/ebin -s btce
