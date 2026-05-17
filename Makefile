.PHONY: init
init:
	git config core.hooksPath .githooks

.PHONY: precommit
precommit:
	lake build
	lake build veir-opt
	lake test
	lit Tests -v

.PHONY: run
run:
	lake exe velang

.PHONY: lit
lit:
	lake build veir-opt
	lit Tests -v
