#
# terminal Makefile @(#)Makefile	1.2 (CWI) 88/03/04
#
DESTDIR =
BIN = /usr/local
LIBDIR = /usr/local/lib/ditroff/term
TABS = tab.1650.10 tab.1650.12 tab.37 tab.450 tab.lp

all:	${TABS}


install: ${TABS} $(LIBDIR)
	for i in ${TABS}; do install -m 644 -c $$i ${LIBDIR}/$$i; done

$(LIBDIR):
	mkdir $(LIBDIR)

clean:
