#	@(#)Makefile	5.1 (Berkeley) 6/18/90
#
# PATCHES MAGIC                LEVEL   PATCH THAT GOT US HERE
# --------------------         -----   ----------------------
# CURRENT PATCH LEVEL:         1       00138
# --------------------         -----   ----------------------
#
# 11 Apr 93	Rodney W. Grimes	Added conditional for profiled
#					library LINKS

LIB=	ln
SRCS=	libmain.c
LINKS=	${LIBDIR}/libln.a ${LIBDIR}/libl.a \

.if !defined(NOPROFILE)
LINKS+=	${LIBDIR}/libln_p.a ${LIBDIR}/libl_p.a
.endif

.include <bsd.lib.mk>
