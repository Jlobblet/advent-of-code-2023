INPUT=../../data/19.txt

DOTNET=dotnet

.PHONY: all run clean build

all: clean build run

run: build
	INPUT=$(INPUT) $(DOTNET) run --project day19cs

build:
	$(DOTNET) build

clean:
	$(DOTNET) clean
