#	@(#)Makefile	5.26 (Berkeley) %G%

SUBDIR=	awk compact cpp crypt dbconv eqn groups hostid ld lex make \
	mkpasswd more ms od refer roff sh sum symorder talk tbl \
	tmac whereis which whoami yacc

.if   (${MACHINE} == "hp300")
SUBDIR+=as.hpux
.elif (${MACHINE} == "tahoe")
SUBDIR+=as.tahoe dbx pcc
.elif ${MACHINE} == "vax"
SUBDIR+=as.vax dbx pcc talk tp
.endif

.include <bsd.subdir.mk>
