#	@(#)Makefile	5.29 (Berkeley) %G%

SUBDIR=	compact hostid lex make yacc

.if   (${MACHINE} == "hp300")
SUBDIR+=as.hpux
.elif (${MACHINE} == "tahoe")
SUBDIR+=as.tahoe dbx pcc
.elif ${MACHINE} == "vax"
SUBDIR+=as.vax dbx pcc talk tp
.endif

.include <bsd.subdir.mk>
