#	@(#)Makefile	5.1 (Berkeley) 6/5/90

#SHLIB_MAJOR= 0
#SHLIB_MINOR= 0

LIB=resolv
CFLAGS+=-DDEBUG -DLIBC_SCCS

.PATH:	${.CURDIR}/../libc/net

SRCS=gethostnamadr.c res_mkquery.c res_query.c res_send.c

.include <bsd.lib.mk>
