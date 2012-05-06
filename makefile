ghc       = ghc
find      = find
env       = env
nosetests = nosetests

ghcflags  = -W -Wall -O2 -threaded -i./src
noseflags = 

SRCFILES = $(shell $(find) ./src -type f -name \*.hs)

compile: ./src/sssfs

.PHONY: try
try: compile
	env PYTHONPATH=./try $(nosetests) $(noseflags) ./try

./src/sssfs: $(SRCFILES)
	$(ghc) $(ghcflags) --make $(CURDIR)/src/sssfs.hs
