# @(#)Makefile	4.1 %G%
ALL=	ftp ftpd
CFLAGS=-O

all:	${ALL}

ftp:	cmds.o cmdtab.o ftp.o main.o
	${CC} cmds.o cmdtab.o ftp.o main.o -o ftp -ljobs

ftp.o:	ftp.h ftp_var.h
cmds.o:	ftp.h ftp_var.h
cmdtab.o: ftp_var.h
ftp.o:	ftp_var.h
main.o: ftp_var.h

ftpd:	ftpd.o ftpcmd.o vers.o glob.o
	${CC} ftpd.o ftpcmd.o glob.o -o ftpd vers.o -ljobs

vers.o:	ftpd.c ftpcmd.y
	sh newvers.sh
	${CC} -c vers.c

clean:
	rm -f ${ALL} *.o *.s errs core a.out t.? yacc.* y.tab.c
