ALLC = apilib.c
ALLH =

ALLO = apilib.o

apilib.a: ${ALLO}
	@-/bin/rm $@
	ar cr $@ ${ALLO}
	ranlib $@
