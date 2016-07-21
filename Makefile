REBAR=`which rebar3 || echo ./rebar3`
BASEDIR=$(dir $(firstword $(MAKEFILE_LIST)))
all: compile docs tests
full: clean-all compile docs tests
ci: compile docs tests

compile:
	@$(REBAR) compile
tests:
	@$(REBAR) eunit
clean:
	@$(REBAR) clean
clean-all:
	@rm -rf $(BASEDIR)_build/*
docs:
	@$(REBAR) edoc