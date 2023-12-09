INPUT=../../data/08.txt

STACK=stack

.PHONY: run build test clean

run: build
	INPUT=$(INPUT) $(STACK) run

test: build
	$(STACK) test

build:
	$(STACK) build

clean:
	$(STACK) clean
