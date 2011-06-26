EBIN_DIR := ebin
SRC_DIR := src
EXAMPLES_DIR := examples
TEST_DIR := test
INCLUDE_DIR := include
ERLC := erlc
ERLC_FLAGS := -W -I $(INCLUDE_DIR) -o $(EBIN_DIR) -pa $(EBIN_DIR)
ERL := erl
TEST_FLAGS := -noshell -pa $(EBIN_DIR) 
TEST_MODULES := spooky_test,spooky_test_session,spooky_test_template

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
	@$(ERLC) $(ERLC_FLAGS) $(SRC_DIR)/*.erl
	@$(ERLC) $(ERLC_FLAGS) $(EXAMPLES_DIR)/*.erl
	@$(ERLC) $(ERLC_FLAGS) $(TEST_DIR)/*.erl
	@$(ERL) $(TEST_FLAGS) -eval "eunit:test([$(TEST_MODULES)],[verbose])." -s init stop


.PHONY: test examples
	
