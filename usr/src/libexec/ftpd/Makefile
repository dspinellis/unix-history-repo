#	@(#)Makefile	4.1	%G%
#
ALL=	ftpd
DESTDIR=
CFLAGS=	-O

all:	${ALL}

ftpd:	ftpd.o ftpcmd.o vers.o glob.o
	${CC} ftpd.o ftpcmd.o glob.o -o ftpd vers.o -ljobs

vers.o:	ftpd.c ftpcmd.y
	sh newvers.sh
	${CC} -c vers.c

install:
	install -s ftpd ${DESTDIR}/etc/ftpd

clean:
	rm -f ${ALL} *.o *.s errs core a.out t.? yacc.* y.tab.c
