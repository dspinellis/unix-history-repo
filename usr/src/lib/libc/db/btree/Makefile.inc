#	@(#)Makefile.inc	5.4 (Berkeley) %G%

.PATH: ${.CURDIR}/db/btree

SRCS+=	bt_close.c bt_conv.c bt_debug.c bt_delete.c bt_get.c bt_open.c \
	bt_overflow.c bt_put.c bt_search.c bt_seq.c bt_split.c bt_stack.c \
	bt_utils.c
