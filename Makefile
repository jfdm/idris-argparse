##  Makefile

IDRIS := idris
LIB   := argparse
OPTS  :=

.PHONY: clean lib clobber check test doc

install:
	${IDRIS} ${OPTS} --install ${LIB}.ipkg

lib:
	${IDRIS} ${OPTS} --build ${LIB}.ipkg

clean:
	${IDRIS} --clean ${LIB}.ipkg
	find . -name "*~" -delete

clobber: clean
	find . -name "*.ibc" -delete

check:
	${IDRIS} --checkpkg ${LIB}.ipkg

test:
	${IDRIS} --testpkg ${LIB}.ipkg

doc:
	${IDRIS} --mkdoc ${LIB}.ipkg

# ---------------------------------------------------------------------- [ EOF ]
