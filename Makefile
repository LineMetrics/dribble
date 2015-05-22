all: erl test

erl:
	rebar get-deps compile

test: all
	rebar skip_deps=true compile ct

clean:
	rebar clean
	rebar -rm -rvf deps ebin doc

dialyzer:
	dialyzer --verbose --plts .plt --src src -r ebin

doc:
	rebar doc

build-plt:
	dialyzer --build_plt --output_plt .plt --apps kernel stdlib erts

.PHONY: all erl test clean dialyzer doc build-plt
