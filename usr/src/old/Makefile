#	@(#)Makefile	5.9 (Berkeley) %G%

SUBDIR=	compact hostid lex make yacc

.if   (${MACHINE} == "tahoe")
SUBDIR+=pcc
.elif ${MACHINE} == "vax"
SUBDIR+=pcc talk tp
.endif

.include <bsd.subdir.mk>
