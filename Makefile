#	@(#)Makefile	5.1.1.2 (Berkeley) 5/9/91
#
# PATCHES MAGIC                LEVEL   PATCH THAT GOT US HERE
# --------------------         -----   ----------------------
# CURRENT PATCH LEVEL:         1       00128
# --------------------         -----   ----------------------
#
# 04 Apr 93	Rodney W. Grimes	Pulled in Net/2 version string
#					Added not ported comment
#					Added special cases comment
#					Added contrib, games, and share
#

SUBDIR=	bin contrib include lib libexec sbin share usr.bin usr.sbin

# Special cases: etc sys.386bsd
# Not ported: games kerberosIV

.include <bsd.subdir.mk>
