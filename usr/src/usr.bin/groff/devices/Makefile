#	@(#)Makefile	6.1 (Berkeley) 3/3/91

#
# Ugly installation glitch: you MUST be running X11 when building
# the xditview stuff, since xtotroff queries the local X server to get
# font information.  Therefore these are not built by default.
#
XDEVICES=	devX100 devX100-12 devX75 devX75-12

SUBDIR=		devascii devlatin1 devdvi devps

.include <bsd.subdir.mk>
