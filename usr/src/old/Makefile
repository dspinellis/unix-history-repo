#	@(#)Makefile	5.35 (Berkeley) %G%

SUBDIR=	awk compact cpp crypt dbconv ed eqn groups hostid lex make \
	mkpasswd more ms od pr refer roff sed sh sum symorder tbl \
	test tmac whereis which whoami yacc

.if   (${MACHINE} == "hp300")
SUBDIR+=adb as.hpux ld
.elif (${MACHINE} == "i386")
SUBDIR+=ld
.elif (${MACHINE} == "luna68k")
SUBDIR+=ld
.elif (${MACHINE} == "mips")
SUBDIR+=ld
.elif (${MACHINE} == "tahoe")
SUBDIR+=adb as.tahoe dbx dlmpcc ld pcc
.elif ${MACHINE} == "vax"
SUBDIR+=adb arff as.vax dbx flcopy ld pcc rxformat talk tp
.endif

.include <bsd.subdir.mk>
