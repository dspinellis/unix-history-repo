#	@(#)Makefile	5.1 (Berkeley) %G%

SUBDIR=	boggle mkdict mkindex

beforeinstall:
	mkdict/obj/mkdict < /usr/share/dict/words > \
	    ${DESTDIR}/usr/share/games/boggle/dictionary
	mkindex/obj/mkindex < ${DESTDIR}/usr/share/games/boggle/dictionary > \
	    ${DESTDIR}/usr/share/games/boggle/dictindex
	chown ${BINOWN}.${BINGRP} ${DESTDIR}/usr/share/games/boggle/dictionary
	chmod 444 ${DESTDIR}/usr/share/games/boggle/dictionary
	chown ${BINOWN}.${BINGRP} ${DESTDIR}/usr/share/games/boggle/dictindex
	chmod 444 ${DESTDIR}/usr/share/games/boggle/dictindex
	
.include <bsd.subdir.mk>
