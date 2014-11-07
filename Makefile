PROJECT=httpre
REBAR=./rebar

all: deps compile script

clean:
	@echo "Running rebar clean..."
	@$(REBAR) clean
	@rm -rf deps ebin

compile:
	@echo "Running rebar compile..."
	@$(REBAR) compile

deps:
	@echo "Running rebar update-deps..."
	@$(REBAR) update-deps

script:
	@echo "Running rebar escriptize..."
	@$(REBAR) escriptize

.PHONY: deps
