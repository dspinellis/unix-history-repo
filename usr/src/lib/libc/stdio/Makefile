#	Makefile	4.8	83/06/30
#
# doprnt is not here because it is machine dependent
#
CFLAGS=	-O

OBJS=	clrerr.o data.o doscan.o \
	exit.o fdopen.o fgetc.o fgets.o filbuf.o flsbuf.o fopen.o \
	fprintf.o fputc.o fputs.o freopen.o fseek.o ftell.o \
	getchar.o gets.o getw.o printf.o putchar.o puts.o \
	putw.o rdwr.o rew.o scanf.o setbuf.o setbuffer.o \
	sprintf.o strout.o ungetc.o

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
