# Makefile %G%
ALL=	tftp
DESTDIR=
CFLAGS=-O

all: ${ALL}

tftp:	main.o tftp.o
	${CC} main.o tftp.o -o tftp

clean:
	rm -f ${ALL} *.o *.s errs core a.out t.?

install: 
	install -s tftp ${DESTDIR}/usr/ucb
