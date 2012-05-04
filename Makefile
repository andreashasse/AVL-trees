.PHONY: 

all:
	./rebar get-deps compile

clean:
	./rebar clean
	find . -name "*~" -exec rm {} \;
	find . -name ".#*" -exec rm {} \;
	find . -name "erl_crash.dump" -exec rm {} \;
	find . -name "#*#" -exec rm {} \;

ct:
	rm -rf apps/*/logs/
	rebar ct skip_deps=true

ct_case:
	rm -rf apps/*/logs/
	rebar ct skip_deps=true suites=${SUITE} case=${CASE}

test:
	rm -rf .eunit
	./rebar eunit skip_deps=true

## add your dependecies here. --apps [depencencies from otp] -r [our deps]
init_dialyzer:
	dialyzer --apps stdlib kernel -r deps --build_plt --output_plt .dialyzer.plt

dialyzer:
	dialyzer -r apps --plt .dialyzer.plt
