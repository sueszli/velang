.PHONY: init
init:
	git config core.hooksPath .githooks

.PHONY: precommit
precommit:
	find . -name '*.lean' -not -path './.lake/*' -print0 | xargs -0 -P 4 -I{} sh -c '{ go run github.com/lotusirous/lean-fmt@latest "$$1"; echo; } > "$$1.tmp" && mv "$$1.tmp" "$$1"' _ {}
	lake build
	lake test
	lake lint

.PHONY: run
run:
	lake exe yung-lean
