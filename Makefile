.PHONY: 

all:
	./rebar get-deps compile

clean:
	./rebar clean
	find . -name "*~" -exec rm {} \;
	find . -name ".#*" -exec rm {} \;
	find . -name "erl_crash.dump" -exec rm {} \;
	find . -name "#*#" -exec rm {} \;

edoc:
	./rebar doc skip_deps=true
test:
	rm -rf .eunit
	./rebar compile eunit skip_deps=true

## add your dependecies here. --apps [depencencies from otp] -r [our deps]
init_dialyzer:
	dialyzer --apps stdlib kernel -r deps --build_plt --output_plt .dialyzer.plt

dialyzer:
	dialyzer -r ebin --plt .dialyzer.plt
