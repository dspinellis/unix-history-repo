#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.3 (Berkeley) %G%
#
# reorder gives an editor command for most common terminals
# (in reverse order from n'th to 1'st most commonly used)
# to move them to the front of termcap
#
DESTDIR =

termcap: reorder termcap.src
	ex - termcap.src < reorder

install: termcap tabset
	install -m 444 termcap ${DESTDIR}/etc/termcap

tabset: FRC
	rm -fr ${DESTDIR}/usr/lib/tabset
	mkdir ${DESTDIR}/usr/lib/tabset
	cp tabset/* ${DESTDIR}/usr/lib/tabset

clean:
	rm -f termcap

depend:

FRC:

installall:
	cp termcap /etc/termcap
	@for i in arpa ernie kim dali calder ucbvax miro;\
		do echo $$i;\
		rcp termcap $$i':'/etc/termcap;\
		done

tabsetall: tabset
	@for i in arpa ernie kim dali calder ucbvax miro;\
		do echo $$i;\
		rsh $$i /bin/rm -fr /usr/lib/tabset;\
		rcp -r tabset $$i':'/usr/lib;\
		done
