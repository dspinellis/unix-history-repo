#	@(#)Makefile.inc	5.1 (Berkeley) %G%
#
.PATH: ${.CURDIR}/db

.include "${.CURDIR}/db/btree/Makefile.inc"
.include "${.CURDIR}/db/hash/Makefile.inc"

MAN3+=	db.0
