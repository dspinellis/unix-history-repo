#       @(#)Makefile.inc	5.3 (Berkeley) %G%

.PATH: ${.CURDIR}/db/man

# mpool.3
MAN3+=	btree.0 dbopen.0 hash.0 recno.0
MLINKS+= dbopen.3 db.3
