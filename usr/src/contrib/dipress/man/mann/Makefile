# Makefile for manual pages for the Unix Interpress utilities
## (C) Copyright 1985, 1986 Xerox Corp.
#
DESTMAN=/usr/man/mann
MANS= charset.1 dipress.1 ipf.1 ipfe.1 ipmetrics.1 iptotext.1 iptroff.1 \
	makextdev.1 maha.1 mp2res.1 plot2ip.1 texttoip.1

all:;
install:;

FRC:

#.SUFFIXES: .1 .n.ip
#.1.n.ip:
#	iptroff -man NoCutMarks.man $<

# Do a "make install" before running this command.
# Calling rdist so many times in not very efficient
rdist:
	for i in ${MANS}; do \
		for j in ${DESTMACHINES}; do \
			(rdist -c ${DESTMAN}/$$i $$j); done; done

${DESTMAN}:
	mkdir ${DESTMAN}

clean:
