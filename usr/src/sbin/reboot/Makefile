#	@(#)Makefile	5.7 (Berkeley) %G%

PROG=	reboot
DPADD=	${LIBUTIL}
LDADD=	-lutil
MAN8=	reboot.0

reboot.0: boot_hp300.0 boot_i386.0 boot_tahoe.0 boot_vax.0

afterinstall:
	${MINSTALL} boot_hp300.0 ${DESTDIR}${MANDIR}8/hp300/boot.0
	${MINSTALL} boot_i386.0 ${DESTDIR}${MANDIR}8/i386/boot.0
	${MINSTALL} boot_tahoe.0 ${DESTDIR}${MANDIR}8/tahoe/boot.0
	${MINSTALL} boot_vax.0 ${DESTDIR}${MANDIR}8/vax/boot.0

.include <bsd.man.mk>
.include <bsd.prog.mk>
