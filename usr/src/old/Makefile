#	@(#)Makefile	5.22 (Berkeley) %G%

SUBDIR=	compact cpp crypt groups hostid ld lex make more refer rev roff \
	sum symorder which whoami yacc

.if   (${MACHINE} == "hp300")
SUBDIR+=as.hpux
.elif (${MACHINE} == "tahoe")
SUBDIR+=as.tahoe dbx pcc
.elif ${MACHINE} == "vax"
SUBDIR+=as.vax dbx pcc talk tp
.endif

.include <bsd.subdir.mk>
