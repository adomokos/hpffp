TOOLS_DIR ?= $(HOME)/.local/bin
GHCID_BIN := $(TOOLS_DIR)/ghcid


$(GHCID_BIN):
	stack install ghcid

ghcid: $(GHCID_BIN) ## Verifies quickly if project compiles on file change
	ghcid --command "make repl"
.PHONY: ghcid

repl: ## Run a REPL for development
	stack repl
.PHONY: repl

run: ## Run the program
	@stack build
	@stack exec -- hpffp-exe
.PHONY: run

help:	## Display this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.PHONY: help
.DEFAULT_GOAL := help
