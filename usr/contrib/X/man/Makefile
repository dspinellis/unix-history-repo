#
# Copyright 1985, Massachusetts Institute of Technology
#
DESTDIR=
MANDIR= /usr/man/man
SECTION=n
TROFF= troff -T480
PRINTER= -Pim

all:

install:
	for i in *.1; do \
		cp $$i ${DESTDIR}${MANDIR}${SECTION}/`basename $$i .1`.${SECTION}; \
	done
	cp *.3* ${DESTDIR}${MANDIR}3
	cp *.8* ${DESTDIR}${MANDIR}8

clean:
	rm -f *~ \#*

paper:
	for i in *.? *.[0-9]?; do \
		echo -n "Troffing ";\
		echo -n $$i;\
		echo '....';\
		${TROFF} -man $$i | lpr -n ${PRINTER};\
	done
