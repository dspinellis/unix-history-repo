# @(#)Makefile	1.8	(Berkeley) %G%
#
# reorder gives an editor command for most common terminals
# (in reverse order from n'th to 1'st most commonly used)
# to move them to the front of termcap
#
DESTDIR =

termcap: reorder termcap.src
	ex - termcap.src < reorder

install: termcap
	install -o root -m 664 -c termcap ${DESTDIR}/etc/termcap
	chgrp termcap ${DESTDIR}/etc/termcap
	rm -fr ${DESTDIR}/usr/lib/tabset
	mkdir ${DESTDIR}/usr/lib/tabset
	cp tabset/* ${DESTDIR}/usr/lib/tabset

clean:
	rm -f termcap

installall:
	cp termcap /etc/termcap
	rcp termcap arpa:/etc/termcap
	rcp termcap ernie:/etc/termcap
	rcp termcap kim:/etc/termcap
	rcp termcap dali:/etc/termcap
	rcp termcap calder:/etc/termcap
	rcp termcap ucbvax:/etc/termcap
