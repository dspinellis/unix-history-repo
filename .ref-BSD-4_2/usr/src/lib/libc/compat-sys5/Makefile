#	%M%	%I%	%E%
#
CFLAGS=	-O
OBJS=	strcatn.o strcmpn.o strcpyn.o tmpnam.o

.c.o:
	${CC} -p ${CFLAGS} -c $*.c
	-ld -X -r $*.o
	mv a.out profiled/$*.o
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

compat-sys5lib compat-sys5lib_p: ${OBJS}
	@echo "building profiled compat-sys5lib"
	@cd profiled; ar cru ../compat-sys5lib_p ${OBJS}
	@echo "buiding normal compat-sys5lib"
	@ar cru compat-sys5lib ${OBJS}

clean:
	rm -f compat-sys5lib compat-sys5lib_p ${OBJS} profiled/*.o
