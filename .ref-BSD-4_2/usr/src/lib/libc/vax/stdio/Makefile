#	Makefile	4.2	83/07/02
#
# printf & co are here because they are machine dependent
#
CFLAGS=	-O

OBJS=	doprnt.o

.c.o:
	${CC} -p ${CFLAGS} -c $*.c
	-ld -X -r $*.o
	mv a.out profiled/$*.o
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

stdiolib stdiolib_p: ${OBJS}
	@echo "building profiled stdiolib"
	@cd profiled; ar cru ../stdiolib_p ${OBJS}
	@echo "building normal stdiolib"
	@ar cru stdiolib ${OBJS}

clean:
	rm -f stdiolib stdiolib_p ${OBJS} profiled/*.o

doprnt.o: doprnt.s
	cp doprnt.s doprnt.c
	cc -E doprnt.c | sed -f mcount.sed | as -o doprnt.o
	-ld -x -r doprnt.o
	mv a.out profiled/doprnt.o
	cc -E doprnt.c | as -o doprnt.o
	-ld -x -r doprnt.o
	mv a.out doprnt.o
	rm -f doprnt.c
