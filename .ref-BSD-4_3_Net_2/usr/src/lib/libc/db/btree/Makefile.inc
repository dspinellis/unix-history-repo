#	@(#)Makefile.inc	5.3 (Berkeley) 2/12/91

# btree sources

.PATH: ${.CURDIR}/db/btree

SRCS+=	big.c btree.c delete.c insert.c lrucache.c lruhash.c lrutils.c \
	search.c seq.c split.c storage.c updutils.c utils.c
