DESTDIR=
#(net2 et al)
DESTMAN= /usr/share/man
CATEXT= 0
#(other)
#DESTMAN= /usr/man
#CATEXT= $$N

SHELL= /bin/sh
MAKE= make
MANROFF= ( tbl | nroff -man )
INSTALL= install

CAT1= dig.cat1 host.cat1 
CAT3= gethostbyname.cat3 getnetent.cat3 resolver.cat3
CAT5= resolver.cat5
CAT7= hostname.cat7 mailaddr.cat7
CAT8= named-xfer.cat8 named.cat8 \
	named.reload.cat8 named.restart.cat8 nslookup.cat8

BASE1= dig host 
BASE3= gethostbyname resolver getnetent
BASE5= resolver
BASE7= hostname mailaddr
BASE8= named-xfer named named.reload named.restart nslookup

CATPAGES= ${CAT1} ${CAT3} ${CAT5} ${CAT7} ${CAT8}

.SUFFIXES: .1 .cat1 .3 .cat3 .5 .cat5 .7 .cat7 .8 .cat8

.1.cat1:
	${MANROFF} <$*.1 >$*.cat1

.3.cat3:
	${MANROFF} <$*.3 >$*.cat3

.5.cat5:
	${MANROFF} <$*.5 >$*.cat5

.7.cat7:
	${MANROFF} <$*.7 >$*.cat7

.8.cat8:
	${MANROFF} <$*.8 >$*.cat8

MFLAGS= DESTDIR="${DESTDIR}" DESTMAN="${DESTMAN}" CATEXT='${CATEXT}'

all: ${CATPAGES}

clean:
	rm -f *~ *.BAK *.CKP *.orig
	rm -f ${CATPAGES}

depend:

install: ${CATPAGES} ${DESTDIR}${DESTMAN}/cat1 ${DESTDIR}${DESTMAN}/cat3 \
	${DESTDIR}${DESTMAN}/cat5 ${DESTDIR}${DESTMAN}/cat7 \
	${DESTDIR}${DESTMAN}/cat8
	@set -x; N=1; for x in ${BASE1}; do \
		${INSTALL} -c -m 444 $$x.cat$$N \
			${DESTDIR}${DESTMAN}/cat$$N/$$x.${CATEXT}; \
	done
	@set -x; N=3; for x in ${BASE3}; do \
		${INSTALL} -c -m 444 $$x.cat$$N \
			${DESTDIR}${DESTMAN}/cat$$N/$$x.${CATEXT}; \
	done
	@set -x; N=5; for x in ${BASE5}; do \
		${INSTALL} -c -m 444 $$x.cat$$N \
			${DESTDIR}${DESTMAN}/cat$$N/$$x.${CATEXT}; \
	done
	@set -x; N=7; for x in ${BASE7}; do \
		${INSTALL} -c -m 444 $$x.cat$$N \
			${DESTDIR}${DESTMAN}/cat$$N/$$x.${CATEXT}; \
	done
	@set -x; N=8; for x in ${BASE8}; do \
		${INSTALL} -c -m 444 $$x.cat$$N \
			${DESTDIR}${DESTMAN}/cat$$N/$$x.${CATEXT}; \
	done

${DESTDIR}${DESTMAN}/cat1 ${DESTDIR}${DESTMAN}/cat3 \
	${DESTDIR}${DESTMAN}/cat5 ${DESTDIR}${DESTMAN}/cat7 \
	${DESTDIR}${DESTMAN}/cat8:; mkdir $@
