#	@(#)Makefile	6.1 (Berkeley) 3/3/91
#
# PATCHES MAGIC                LEVEL   PATCH THAT GOT US HERE
# --------------------         -----   ----------------------
# CURRENT PATCH LEVEL:         1       00120
# --------------------         -----   ----------------------
#
# 30 Mar 93	Rodney W. Grimes	Added null tags and depend targets
#

#
# Ugly installation glitch: you MUST be running X11 when building
# the xditview stuff, since xtotroff queries the local X server to get
# font information.  Therefore these are not built by default.
#
XDEVICES=	devX100 devX100-12 devX75 devX75-12

SUBDIR=		devascii devlatin1 devdvi devps

tags:

depend:

.include <bsd.subdir.mk>
