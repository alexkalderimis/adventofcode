YEARS = 2015 2017 2018 2019

SOURCES := $(shell find $(YEARS) -depth 2 -type f -name 'Main.hs')
FILES := $(SOURCES:.hs=)

adventofcode.cabal: package.yaml stack.yaml
	stack build

%/Main: %/Main.hs adventofcode.cabal
	stack ghc -- -O2 $< -o $@

all: $(FILES)

clean:
	rm -f $(FILES)

.PHONY: clean
