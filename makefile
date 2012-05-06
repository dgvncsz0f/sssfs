ghc       = ghc
find      = find
env       = env
nosetests = nosetests

ghcflags  = -W -Wall -O2 -threaded -i./src
noseflags = 

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
	$(find) ./src -type f -name \*.o -exec rm -f \{\} \;
	$(find) ./src -type f -name \*.hi -exec rm -f \{\} \;
	$(find) ./try -type f -name \*.pyc -exec rm -f \{\} \;

$(SSSFS): $(SRCFILES)
	$(ghc) $(ghcflags) --make $(SSSFS).hs

