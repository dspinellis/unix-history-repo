#	@(#)Makefile	5.6 (Berkeley) %G%

MAN8=	adduser.0 intro.0 makedev.0 rc.0 sticky.0
MLINKS=	sa.8 accton.8 mount.8 umount.8 newfs.8 mkfs.8 quotaon.8 quotaoff.8 \
	uucico.8 uucpd.8 fastboot.8 fasthalt.8
SUBDIR=	man8.vax

all: ${MAN8}

clean depend lint tags:

cleandir:
	rm -f ${MAN8}

install: maninstall

.include <bsd.prog.mk>
