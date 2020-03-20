SOURCES := $(wildcard */*.hs)

.PHONY: build
build:
	stack build
	$(MAKE) -C web

.PHONY: install
install:
	stack install

.PHONY: format
format:
	stylish-haskell --inplace $(SOURCES)
	$(MAKE) format -C web

.PHONY: test
test:
	stack test --fast

.PHONY: test.watch
test.watch:
	stack test --test --file-watch --fast
