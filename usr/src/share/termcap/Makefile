# @(#)Makefile	1.5	%G%
#
# reorder gives an editor command for most common terminals
# (in reverse order from n'th to 1'st most commonly used)
# to move them to the front of termcap
#
DESTDIR =

termcap.obj: reorder termcap.src
	ex - termcap.src < reorder

install: termcap.obj
	install -c termcap.obj ${DESTDIR}/etc/termcap
	rm -fr ${DESTDIR}/usr/lib/tabset
	mkdir ${DESTDIR}/usr/lib/tabset
	cp tabset/* ${DESTDIR}/usr/lib/tabset

clean:
	rm -f termcap.obj
