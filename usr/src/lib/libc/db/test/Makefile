#	@(#)Makefile	5.2 (Berkeley) %G%

PROG=	dbtest
SRCS=	dbtest.c
#SRCS+=	rec_close.c rec_delete.c rec_get.c rec_open.c rec_put.c \
#	rec_search.c rec_seq.c rec_utils.c
#.PATH:	${.CURDIR}/../recno
#SRCS+=	bt_close.c bt_conv.c bt_debug.c bt_delete.c bt_get.c bt_open.c \
#	bt_overflow.c bt_put.c bt_search.c bt_seq.c bt_split.c bt_stack.c \
#	bt_utils.c
#.PATH:	${.CURDIR}/../btree
CFLAGS=	-g -DDEBUG
NOMAN=	noman

NOINCLUDE=1
.include <bsd.prog.mk>
