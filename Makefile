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
	$(MAKE) -C web format

.PHONY: test
test:
	stack test --fast

.PHONY: test.watch
test.watch:
	stack test --test --file-watch --fast

.PHONY: start
start:
	$(MAKE) -C web start
