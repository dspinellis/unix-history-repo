#	@(#)Makefile	4.4	%G%
#
ALL=	ftp
DESTDIR=
CFLAGS=	-O

all:	${ALL}

ftp:	cmds.o cmdtab.o ftp.o glob.o main.o
	${CC} cmds.o cmdtab.o ftp.o glob.o main.o -o ftp -ljobs

ftp.o:	/usr/include/arpa/ftp.h ftp_var.h
cmds.o:	/usr/include/arpa/ftp.h ftp_var.h
cmdtab.o: ftp_var.h
ftp.o:	ftp_var.h
main.o: ftp_var.h

install:
	install -s ftp ${DESTDIR}/usr/ucb/ftp

clean:
	rm -f ${ALL} *.o *.s errs core a.out t.?
