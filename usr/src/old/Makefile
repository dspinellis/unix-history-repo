#	@(#)Makefile	5.31 (Berkeley) %G%

SUBDIR=	awk compact cpp crypt dbconv ed eqn groups hostid ld lex make \
	mkpasswd more ms od refer roff sed sh sum symorder tbl test tmac \
	whereis which whoami yacc

.if   (${MACHINE} == "hp300")
SUBDIR+=as.hpux
.elif (${MACHINE} == "tahoe")
SUBDIR+=as.tahoe dbx pcc
.elif ${MACHINE} == "vax"
SUBDIR+=arff as.vax dbx flcopy pcc rxformat talk tp
.endif

.include <bsd.subdir.mk>
