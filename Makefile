YEARS = 2015/ 2017/ 2018/ 2019/ 2020/ 2021/

SOURCES := $(shell find $(YEARS) -depth 2 -type f -name 'Main.hs')
FILES := $(SOURCES:.hs=)

SRC_FILES := $(shell find src -name '*.hs')

adventofcode.cabal: package.yaml stack.yaml .last-lib-build
	stack build

%/Main: %/Main.hs adventofcode.cabal .last-lib-build
	stack ghc -- -O2 $< -o $@

.last-lib-build: $(SRC_FILES)
	stack build
	date > .last-lib-build

all: $(FILES)

clean:
	rm -f $(FILES)

.PHONY: clean
