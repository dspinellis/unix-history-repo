#	Makefile	4.1	83/06/27
#
OBJS=	htonl.o htons.o ntohl.o ntohs.o
CFLAGS=	

.c.o:
	/lib/cpp -E -DPROF $*.c | ${AS} -o $*.o
	-ld -x -r $*.o
	mv a.out profiled/$*.o
	/lib/cpp -E $*.c | ${AS} -o $*.o
	-ld -x -r $*.o
	mv a.out $*.o

netlib netlib_p: ${OBJS}
	@echo "building profiled netlib"
	@cd profiled; ar cru ../netlib_p ${OBJS}
	@echo "building normal netlib"
	@ar cru netlib ${OBJS}

clean:
	rm -f ${OBJS} profiled/*.o errs a.out core netlib netlib_p
