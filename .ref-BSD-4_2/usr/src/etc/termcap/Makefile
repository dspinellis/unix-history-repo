# @(#)Makefile	1.10	(Berkeley) 7/1/83
#
# reorder gives an editor command for most common terminals
# (in reverse order from n'th to 1'st most commonly used)
# to move them to the front of termcap
#
DESTDIR =

termcap: reorder termcap.src
	ex - termcap.src < reorder

install: termcap tabset
	install -m 664 termcap ${DESTDIR}/etc/termcap

tabset:
	rm -fr ${DESTDIR}/usr/lib/tabset
	mkdir ${DESTDIR}/usr/lib/tabset
	cp tabset/* ${DESTDIR}/usr/lib/tabset

clean:
	rm -f termcap

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
