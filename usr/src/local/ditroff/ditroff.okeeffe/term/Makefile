#
# terminal Makefile @(#)Makefile	1.1 (CWI) 87/04/27
#
DESTDIR =
BIN = /usr/local
LIBDIR = /usr/local/lib/ditroff/term
TABS = tab.1650.10 tab.1650.12 tab.37 tab.450 tab.lp

all:	${TABS}


install: ${TABS}
	for i in ${TABS}; do install -m 644 -c $$i ${LIBDIR}/$$i; done
clean:
