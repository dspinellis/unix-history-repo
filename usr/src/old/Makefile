#	@(#)Makefile	5.11 (Berkeley) %G%

SUBDIR=	compact cpp crypt hostid lex make more yacc

.if   (${MACHINE} == "hp300")
SUBDIR+=as.hpux
.elif (${MACHINE} == "tahoe")
SUBDIR+=as.tahoe cc dbx pcc
.elif ${MACHINE} == "vax"
SUBDIR+=as.vax cc dbx pcc talk tp
.endif

.include <bsd.subdir.mk>
