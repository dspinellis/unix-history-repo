#	@(#)Makefile	5.33 (Berkeley) %G%

SUBDIR=	compact hostid lex make yacc

.if   (${MACHINE} == "hp300")
SUBDIR+=as.hpux ld
.elif (${MACHINE} == "i386")
SUBDIR+=ld
.elif (${MACHINE} == "luna68k")
SUBDIR+=ld
.elif (${MACHINE} == "mips")
SUBDIR+=ld
.elif (${MACHINE} == "tahoe")
SUBDIR+=as.tahoe dbx dlmpcc ld pcc
.elif ${MACHINE} == "vax"
SUBDIR+=arff as.vax dbx flcopy ld pcc rxformat talk tp
.endif

.include <bsd.subdir.mk>
