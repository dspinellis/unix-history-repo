#	@(#)Makefile	5.23 (Berkeley) %G%

SUBDIR=	awk compact cpp crypt groups hostid ld lex make more refer roff \
	sed sum symorder which whoami yacc

.if   (${MACHINE} == "hp300")
SUBDIR+=as.hpux
.elif (${MACHINE} == "tahoe")
SUBDIR+=as.tahoe dbx pcc
.elif ${MACHINE} == "vax"
SUBDIR+=as.vax dbx pcc talk tp
.endif

.include <bsd.subdir.mk>
