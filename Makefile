all:
	./rebar compile

examples: 
	erlc -pa ebin/ -o ebin/ examples/*erl

test:
	./rebar compile
	erlc -pa ebin/ -o ebin/ examples/*erl
	./rebar eunit

.PHONY: test
	
