#	@(#)Makefile	5.1 (Berkeley) %G%

PROG=	fsinfo
NOMAN=	true
SRCS=	fsinfo.c fsi_gram.c fsi_lex.c \
	fsi_util.c fsi_analyze.c fsi_dict.c \
	wr_atab.c wr_bparam.c wr_dumpset.c \
	wr_exportfs.c wr_fstab.c
CFLAGS+=-I${.CURDIR}/../include
CFLAGS+=-I${.CURDIR}/../rpcx
CFLAGS+=-I${.CURDIR}/../config
CFLAGS+=-DOS_HDR=\"os-bsd44.h\"

fsi_lex.o fsinfo.o: fsi_gram.h
fsi_gram.c fsi_gram.h: ../fsinfo/fsi_gram.y
	@echo "# expect 2 shift/reduce conflicts"
	${YACC} -d ../fsinfo/fsi_gram.y
	mv y.tab.c fsi_gram.c
	mv y.tab.h fsi_gram.h

fsi_lex.c: ../fsinfo/fsi_lex.l
	${LEX} ../fsinfo/fsi_lex.l
	mv lex.yy.c fsi_lex.c

.PATH: ${.CURDIR}/../config
.include "../../Makefile.inc"
.include <bsd.prog.mk>
