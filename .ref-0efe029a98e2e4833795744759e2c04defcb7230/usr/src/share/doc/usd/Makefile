#	@(#)Makefile	5.1 (Berkeley) %G%

# 08.mh		doesn't work right
# 11.notes	deprecate
BINDIR=	/usr/share/doc/usd
FILES=	00.contents Makefile Title
SUBDIR=	01.begin 02.learn 03.shell 04.csh 05.dc 06.bc 07.Mail \
	09.newsread 10.etiq 12.edtut 13.edadv 14.edit 15.vi \
	16.ex 17.jove 18.sed 19.awk 20.msmacros 21.msdiffs 22.memacros \
	23.meref 24.troff 25.trofftut 26.eqn 27.eqnguide 28.tbl 29.refer \
	30.invert 31.bib 32.diction 33.rogue 34.trek

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${FILES} \
	    ${DESTDIR}${BINDIR}

.include <bsd.subdir.mk>
