# 1.1	(CWI)	87/03/31
DESTDIR	=
BINDIR	= /usr/local
LIBDIR	= /usr/local/lib/chem
MANDIR	= /usr/man/manl


all:	chem chem.awk chem.macros chem.l

install:	${DESTDIR}${LIBDIR} all
	install -c -m 755 chem ${DESTDIR}${BINDIR}/chem
	install -c -m 644 chem.awk ${DESTDIR}${LIBDIR}/chem.awk
	install -c -m 644 chem.macros ${DESTDIR}${LIBDIR}/chem.macros
	install -c -m 644 chem.l ${DESTDIR}${MANDIR}/chem.l

${DESTDIR}${LIBDIR}:
	mkdir $@

chem:	chem.sh
	sed 's@chem.awk@${LIBDIR}/&@' < chem.sh > chem

chem.awk:	chem.awk.src
	sed 's@chem.macros@${LIBDIR}/&@' < chem.awk.src > chem.awk

chem.l:	chem.l.src
	sed -e 's@chem.awk@${LIBDIR}/&@' -e 's@chem.macros@${LIBDIR}/&@' < chem.l.src > chem.l

chem.macros:
	cp chem.macros.src chem.macros

clean:
	rm -rf chem chem.awk chem.macros chem.l
