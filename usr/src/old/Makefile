#	@(#)Makefile	5.32 (Berkeley) %G%

SUBDIR=	compact hostid lex make yacc

.if   (${MACHINE} == "hp300")
SUBDIR+=as.hpux
.elif (${MACHINE} == "tahoe")
SUBDIR+=as.tahoe dbx dlmpcc pcc
.elif ${MACHINE} == "vax"
SUBDIR+=arff as.vax dbx flcopy pcc rxformat talk tp
.endif

.include <bsd.subdir.mk>
