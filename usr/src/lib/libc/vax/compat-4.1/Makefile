#	%M%	%I%	%E%
#
OBJS=	reset.o
CFLAGS=

.c.o:
	/lib/cpp -E -DPROF $*.c | ${AS} -o $*.o
	-ld -X -r $*.o
	mv a.out profiled/$*.o
	/lib/cpp -E $*.c | ${AS} -o $*.o
	-ld -x -r $*.o
	mv a.out $*.o

compat-4.1lib compat-4.1lib_p: ${OBJS}
	@echo "building profiled compat-4.1"
	@cd profiled; ar cru ../compat-4.1lib_p ${OBJS}
	@echo "building normal compat-4.1"
	@ar cru compat-4.1lib ${OBJS}

clean:
	rm -f ${OBJS} profiled/*.o errs a.out core compat-4.1lib compat-4.1lib_p
