#	@(#)Makefile	8.1 (Berkeley) %G%

LIB=	kdb
CFLAGS+=-DKERBEROS -DDEBUG -I${.CURDIR}/../include
SRCS=	krb_cache.c krb_dbm.c krb_kdb_utils.c krb_lib.c print_princ.c

NOPROFILE=
.include <bsd.lib.mk>
