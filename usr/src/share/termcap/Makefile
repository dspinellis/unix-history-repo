# @(#)Makefile	1.4	%G%
#
# reorder gives an editor command for most common terminals
# (in reverse order from n'th to 1'st most commonly used)
# to move them to the front of termcap
#
DESTDIR =
termcap.obj: reorder termcap.src
	ex - termcap.src < reorder

install: termcap.obj
	install termcap.obj ${DESTDIR}/etc/termcap

clean:
	rm -f termcap.obj

tabset:
	-mkdir /usr/lib/tabset
	cp tabset/* /usr/lib/tabset

VGRIND=	csh /usr/ucb/vgrind
vgrind:
	${VGRIND} -n -h "Terminal Capibility Data Base" \
		    termcap.src reorder Makefile
