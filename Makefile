EBIN_DIR := ebin
SRC_DIR := src
EXAMPLES_DIR := examples
INCLUDE_DIR := include
ERLC := erlc
ERLC_FLAGS := -W -I $(INCLUDE_DIR) -o $(EBIN_DIR) -pa $(EBIN_DIR)

all:
	./rebar compile

clean:
	@rm -rf $(EBIN_DIR)/*
	@rm -f erl_crash.dump

debug: 
	@mkdir -p $(EBIN_DIR)
	$(ERLC) -D log_debug $(ERLC_FLAGS) $(SRC_DIR)/*.erl

examples: all
	$(ERLC) $(ERLC_FLAGS) $(EXAMPLES_DIR)/*.erl

test:
	$(ERLC) -D log_debug $(ERLC_FLAGS) $(SRC_DIR)/*.erl
	$(ERLC) -D log_debug $(ERLC_FLAGS) $(EXAMPLES_DIR)/*.erl
	./rebar eunit


.PHONY: test examples
	
