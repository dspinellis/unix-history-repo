#
# Copyright (c) 1984, 1985 Xerox Corp.
#
#
BINS= charset dipress ipf ipmetrics iptotext iptroff maha makextdev qip restotext stackres texttoip

install: iptroff
	for i in ${BINS}; do \
		(install -c $$i ${DESTBIN}/$$i); done

iptroff: ../src/bin/iptroff.skel
	sed "2s;^#;# THIS FILE WAS AUTOMATICALLY PRODUCED;" < ../src/bin/iptroff.skel > iptroff.skel0
	sed "s;DESTBIN;${DESTBIN};" < iptroff.skel0 > iptroff.skel1
	sed "s;DESTLIB;${DESTLIB};" < iptroff.skel1 > iptroff.skel2
	sed "s;TITROFFBINARY;${TITROFFBINARY};" < iptroff.skel2 > iptroff
	install -c -m 755 iptroff ${DESTBIN}
	rm -f iptroff.skel0 iptroff.skel1 iptroff.skel2

clean:
	rm -f  ${BINS}
