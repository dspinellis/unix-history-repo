#	@(#)Makefile	5.15 (Berkeley) %G%

SUBDIR=	compact cpp crypt hostid lex make more sum sysline yacc

.if   (${MACHINE} == "hp300")
SUBDIR+=as.hpux
.elif (${MACHINE} == "tahoe")
SUBDIR+=as.tahoe dbx ld pcc
.elif ${MACHINE} == "vax"
SUBDIR+=as.vax dbx ld pcc talk tp
.endif

.include <bsd.subdir.mk>
