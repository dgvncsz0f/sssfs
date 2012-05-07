ghc       = ghc
cabal     = cabal
find      = find
env       = env
nosetests = nosetests

ghcflags             =
noseflags            =
cabal_configureflags =
cabal_buildflags     =

SRCFILES = $(shell $(find) ./src -type f -name \*.hs)
SSSFS    = ./src/sssfs

.PHONY: compile
compile: $(SSSFS)

.PHONY: try
try: tests=./try
try: compile
	$(env) PYTHONPATH=./try $(nosetests) $(noseflags) $(tests)

.PHONY: clean
clean:
	rm -f $(SSSFS)
	$(cabal) clean
	$(find) ./try -type f -name \*.pyc -exec rm -f \{\} \;

$(SSSFS): $(SRCFILES)
	$(cabal) configure $(cabal_configureflags)
	$(cabal) build $(cabal_buildflags)
