#	@(#)Makefile	6.1 (Berkeley) 3/3/91

PROG=		xtotroff
LDADD+=		$(.CURDIR)/../libXdvi/obj/libXdvi.a -lXext -lX11 -lm
CFLAGS+=	-I$(.CURDIR)/.. -L/usr/lib/X11
NOMAN=		noman

install:

.include <bsd.prog.mk>
