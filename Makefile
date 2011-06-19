all:
	./rebar compile

_examples:
	erlc -pa ebin/ -o ebin/ examples/*erl

examples: all _examples

test: _examples
	./rebar compile eunit
	
