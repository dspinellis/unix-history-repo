#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#
ALL=	ftp
DESTDIR=
CFLAGS=	-O

all:	${ALL}

ftp:	cmds.o cmdtab.o ftp.o getpass.o glob.o main.o
	${CC} cmds.o cmdtab.o ftp.o getpass.o glob.o main.o -o ftp 

ftp.o:	/usr/include/arpa/ftp.h ftp_var.h
cmds.o:	/usr/include/arpa/ftp.h ftp_var.h
cmdtab.o: ftp_var.h
ftp.o:	ftp_var.h
main.o: ftp_var.h

install:
	install -s ftp ${DESTDIR}/usr/ucb/ftp

clean:
	rm -f ${ALL} *.o *.s errs core a.out t.?
