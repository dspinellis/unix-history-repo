#	@(#)Makefile	5.1 (Berkeley) 6/25/90

LIB=	kdb
CFLAGS+=-DKERBEROS -DDEBUG -I${.CURDIR}/../include
SRCS=	krb_cache.c krb_dbm.c krb_kdb_utils.c krb_lib.c print_princ.c

.include <bsd.lib.mk>
