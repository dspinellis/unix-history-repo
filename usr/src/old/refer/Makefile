#	@(#)Makefile	4.8	(Berkeley)	87/06/18
#
DESTDIR=
CFLAGS=	-O

all:	mkey inv hunt refer addbib lookbib sortbib

mkey: mkey1.o mkey2.o mkey3.o deliv2.o
	${CC} mkey?.o deliv2.o -o $@

inv: inv1.o inv2.o inv3.o inv5.o inv6.o deliv2.o
	${CC} inv?.o deliv2.o -o $@

hunt: hunt1.o hunt2.o hunt3.o hunt5.o hunt6.o hunt7.o glue5.o
hunt: refer3.o hunt9.o shell.o deliv2.o hunt8.o glue4.o tick.o
	${CC} hunt?.o refer3.o glue5.o glue4.o shell.o deliv2.o tick.o -o $@

refer: glue1.o refer1.o refer2.o refer4.o refer5.o refer6.o mkey3.o
refer: refer7.o refer8.o hunt2.o hunt3.o deliv2.o hunt5.o hunt6.o hunt8.o
refer: glue3.o hunt7.o hunt9.o glue2.o glue4.o glue5.o refer0.o shell.o
	${CC} glue?.o refer[01245678].o hunt[2356789].o mkey3.o shell.o \
		deliv2.o -o $@

addbib: addbib.o
	${CC} addbib.o -o $@

lookbib: lookbib.o
	${CC} lookbib.o -o $@

sortbib: sortbib.o
	${CC} sortbib.o -o $@

whatabout: what1.o what2.o what3.o what4.o shell.o mkey3.o
	${CC} what?.o shell.o mkey3.o -o $@

deliv: deliv1.o deliv2.o
	${CC} deliv?.o -o $@

refpart: refer0.o refer1.o refer2.o refer3.o refer4.o refer5.o
refpart: refer6.o refer7.o refer8.o deliv2.o glue4.o
	${CC} refer?.o deliv2.o glue4.o -o $@

install: all
	-mkdir /usr/lib/refer
	install mkey $(DESTDIR)/usr/lib/refer/mkey
	install inv  $(DESTDIR)/usr/lib/refer/inv
	install hunt $(DESTDIR)/usr/lib/refer/hunt
	install refer $(DESTDIR)/usr/bin/refer
	install addbib $(DESTDIR)/usr/bin/addbib
	install sortbib $(DESTDIR)/usr/bin/sortbib
	install -c roffbib.sh $(DESTDIR)/usr/bin/roffbib
	install -c indxbib.sh $(DESTDIR)/usr/bin/indxbib
	install lookbib $(DESTDIR)/usr/bin/lookbib
	install -c tmac.bib $(DESTDIR)/usr/lib/tmac
	(cd /usr/dict/papers; /bin/sh runinv)

clean:
	rm -f ${all} *.o errs core

depend: ${SOURCES}
	cc -M ${CFLAGS} *.c | sed -e 's, ./, ,g' | \
	    awk ' { if ($$1 != prev) \
		{ if (rec != "") print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		else rec = rec " " $$2 } } \
		END { print rec } ' >makedep
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	ed - Makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it

addbib.o: addbib.c /usr/include/stdio.h /usr/include/ctype.h
addbib.o: /usr/include/signal.h
annobib.o: annobib.c /usr/include/stdio.h
deliv1.o: deliv1.c /usr/include/stdio.h
deliv2.o: deliv2.c /usr/include/stdio.h
flagger.o: flagger.c /usr/include/stdio.h
glue1.o: glue1.c /usr/include/stdio.h
glue2.o: glue2.c
glue3.o: glue3.c refer..c /usr/include/stdio.h /usr/include/ctype.h
glue3.o: /usr/include/assert.h
glue4.o: glue4.c /usr/include/stdio.h /usr/include/ctype.h
glue5.o: glue5.c /usr/include/stdio.h /usr/include/ctype.h
hunt1.o: hunt1.c /usr/include/stdio.h /usr/include/assert.h
hunt2.o: hunt2.c refer..c /usr/include/stdio.h /usr/include/ctype.h
hunt2.o: /usr/include/assert.h
hunt3.o: hunt3.c refer..c /usr/include/stdio.h /usr/include/ctype.h
hunt3.o: /usr/include/assert.h
hunt5.o: hunt5.c /usr/include/stdio.h /usr/include/sys/types.h
hunt5.o: /usr/include/sys/stat.h
hunt6.o: hunt6.c /usr/include/stdio.h /usr/include/assert.h
hunt7.o: hunt7.c /usr/include/stdio.h /usr/include/assert.h
hunt8.o: hunt8.c /usr/include/stdio.h /usr/include/assert.h
hunt9.o: hunt9.c
inv1.o: inv1.c /usr/include/stdio.h /usr/include/assert.h
inv2.o: inv2.c /usr/include/stdio.h /usr/include/assert.h
inv3.o: inv3.c
inv5.o: inv5.c /usr/include/stdio.h
inv6.o: inv6.c /usr/include/stdio.h /usr/include/assert.h
kaiser.o: kaiser.c /usr/include/stdio.h /usr/include/ctype.h
lookbib.o: lookbib.c /usr/include/stdio.h /usr/include/ctype.h
mkey1.o: mkey1.c /usr/include/stdio.h
mkey2.o: mkey2.c /usr/include/stdio.h /usr/include/ctype.h
mkey3.o: mkey3.c /usr/include/stdio.h
refer..o: refer..c /usr/include/stdio.h /usr/include/ctype.h
refer..o: /usr/include/assert.h
refer0.o: refer0.c refer..c /usr/include/stdio.h /usr/include/ctype.h
refer0.o: /usr/include/assert.h
refer1.o: refer1.c /usr/include/signal.h refer..c /usr/include/stdio.h
refer1.o: /usr/include/ctype.h /usr/include/assert.h
refer2.o: refer2.c refer..c /usr/include/stdio.h /usr/include/ctype.h
refer2.o: /usr/include/assert.h
refer3.o: refer3.c refer..c /usr/include/stdio.h /usr/include/ctype.h
refer3.o: /usr/include/assert.h
refer4.o: refer4.c refer..c /usr/include/stdio.h /usr/include/ctype.h
refer4.o: /usr/include/assert.h
refer5.o: refer5.c refer..c /usr/include/stdio.h /usr/include/ctype.h
refer5.o: /usr/include/assert.h
refer6.o: refer6.c refer..c /usr/include/stdio.h /usr/include/ctype.h
refer6.o: /usr/include/assert.h
refer7.o: refer7.c refer..c /usr/include/stdio.h /usr/include/ctype.h
refer7.o: /usr/include/assert.h
refer8.o: refer8.c refer..c /usr/include/stdio.h /usr/include/ctype.h
refer8.o: /usr/include/assert.h
shell.o: shell.c
sortbib.o: sortbib.c /usr/include/stdio.h /usr/include/signal.h
thash.o: thash.c /usr/include/stdio.h
tick.o: tick.c /usr/include/stdio.h /usr/include/sys/types.h
tick.o: /usr/include/sys/timeb.h
types.o: types.c
what..o: what..c /usr/include/stdio.h /usr/include/ctype.h
what..o: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/assert.h
what1.o: what1.c what..c /usr/include/stdio.h /usr/include/ctype.h
what1.o: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/assert.h
what2.o: what2.c /usr/include/stdio.h /usr/include/ctype.h
what3.o: what3.c what..c /usr/include/stdio.h /usr/include/ctype.h
what3.o: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/assert.h
what4.o: what4.c what..c /usr/include/stdio.h /usr/include/ctype.h
what4.o: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/assert.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
