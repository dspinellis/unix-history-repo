#	@(#)Makefile	5.1 (Berkeley) %G%

SRCS=	lrucache.h btree.h big.c btree.c delete.c insert.c lrucache.c \
	lruhash.c lrutils.c search.c seq.c split.c storage.c \
	updutils.c utils.c

OBJS=	big.o btree.o delete.o insert.o lrucache.o \
	lruhash.o lrutils.o search.o seq.o split.o storage.o \
	updutils.o utils.o

#CFLAGS=	-O
CFLAGS=	-g

libbtree.a: $(OBJS)
	ar ru libbtree.a $(OBJS)
	ranlib libbtree.a
