#	@(#)Makefile	5.5 (Berkeley) %G%

SUBDIR=	nroff troff nroff_term
FONTS=	fontlist ftB ftBC ftC ftCE ftCI ftCK ftCW ftFD ftG ftGI ftGM \
	ftGR ftH ftHI ftHM ftI ftI.bak ftL ftLI ftPA ftPB ftPI ftR \
	ftS ftSB ftSI ftSM ftTB ftTI ftTR ftTS ftUD ftap ftbb ftbi ftbk \
	ftbr ftcb ftch ftch.old ftci ftcl ftco ftcr ftct ftcy ftdb ftdi \
	ftdr ftfx ftgb ftgf ftgi ftgk ftgr fthb fthn ftm ftmb ftmi ftmn \
	ftmr ftnb ftni ftnr ftob ftoe ftoi ftor ftpb ftpp ftsb ftsc ftsg \
	ftsh ftsi ftsl ftsr fttB fttI fttR fttS fttb ftti fttr ftts ftwp \
	ftxx

beforeinstall:
	cd ${.CURDIR}/fonts; \
	    install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${FONTS} \
	    ${DESTDIR}/usr/old/libdata/troff_font

.include <bsd.subdir.mk>
