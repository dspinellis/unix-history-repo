# Makefile %G%
ALL=	tftp tftpd
DESTDIR=
CFLAGS=-O

all: ${ALL}

tftp:	main.o tftp.o
	${CC} main.o tftp.o -o tftp -ljobs

tftpd:	tftpd.o
	${CC} tftpd.o -o tftpd -ljobs

clean:
	rm -f ${ALL} *.o *.s errs core a.out t.?

install: tftp tftpd
	install -s tftp ${DESTDIR}/usr/ucb
	install -s tftpd ${DESTDIR}/etc
