# Makefile for manual pages for the Unix Interpress utilities
#
# (C) Copyright 1985, Xerox Corp.
#
DESTMAN=/usr/man/mann
.SUFFIXES: .1 .1.cat

all: charset.1.cat dipress.1.cat iptotext.1.cat iptroff.1.cat maha.1.cat texttoip.1.cat

.1.1.cat:
	iptroff -man $< 

install:
	install -c charset.1 ${DESTMAN}
	install -c dipress.1 ${DESTMAN}
	install -c iptotext.1 ${DESTMAN}
	install -c iptroff.1 ${DESTMAN}
	install -c maha.1 ${DESTMAN}
	install -c texttoip.1 ${DESTMAN}

clean:
