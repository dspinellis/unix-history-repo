#	@(#)Makefile	4.5	(Berkeley)	84/06/05
#
DESTDIR=
CFLAGS = -O

all:	mkey inv hunt refer addbib lookbib sortbib

mkey: mkey1.o mkey2.o mkey3.o deliv2.o
	cc mkey?.o deliv2.o -o mkey
inv: inv1.o inv2.o inv3.o inv5.o inv6.o deliv2.o
	cc inv?.o deliv2.o -o inv
hunt: hunt1.o hunt2.o hunt3.o hunt5.o hunt6.o hunt7.o glue5.o
hunt: refer3.o hunt9.o shell.o deliv2.o hunt8.o glue4.o tick.o
	cc hunt?.o refer3.o glue5.o glue4.o shell.o deliv2.o tick.o -o hunt

glue3.o: refer..c
hunt2.o: refer..c
hunt3.o: refer..c
refer0.o: refer..c
refer1.o: refer..c
refer2.o: refer..c
refer3.o: refer..c
refer4.o: refer..c
refer6.o: refer..c
refer5.o: refer..c
refer: glue1.o refer1.o refer2.o refer4.o refer5.o refer6.o mkey3.o
refer: refer7.o refer8.o hunt2.o hunt3.o deliv2.o hunt5.o hunt6.o hunt8.o
refer: glue3.o hunt7.o hunt9.o glue2.o glue4.o glue5.o refer0.o shell.o
	cc glue?.o refer[01245678].o hunt[2356789].o mkey3.o shell.o deliv2.o -o refer

addbib: addbib.o
	cc addbib.o -o addbib
lookbib: lookbib.o
	cc lookbib.o -o lookbib
sortbib: sortbib.o
	cc sortbib.o -o sortbib

install: all
	install -s mkey $(DESTDIR)/usr/lib/refer
	install -s inv  $(DESTDIR)/usr/lib/refer
	install -s hunt $(DESTDIR)/usr/lib/refer
	install -s refer $(DESTDIR)/usr/bin
	install -s addbib $(DESTDIR)/usr/bin
	install -s sortbib $(DESTDIR)/usr/bin
	install -c roffbib.sh $(DESTDIR)/usr/bin/roffbib
	install -c indxbib.sh $(DESTDIR)/usr/bin/indxbib
	install -s lookbib $(DESTDIR)/usr/bin
	install -c tmac.bib $(DESTDIR)/usr/lib/tmac
clean:
	rm -f refer inv hunt mkey addbib lookbib sortbib *.o

whatabout: what1.o what2.o what3.o what4.o shell.o mkey3.o
	cc what?.o shell.o mkey3.o  -o whatabout
deliv: deliv1.o deliv2.o
	cc deliv?.o  -o deliv
refpart: refer0.o refer1.o refer2.o refer3.o refer4.o refer5.o
refpart: refer6.o refer7.o refer8.o deliv2.o glue4.o
	cc refer?.o deliv2.o glue4.o  -o refpart
