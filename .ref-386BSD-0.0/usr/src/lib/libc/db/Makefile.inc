#	@(#)Makefile.inc	5.1 (Berkeley) 2/24/91
#
.PATH: ${.CURDIR}/db

.include "${.CURDIR}/db/btree/Makefile.inc"
.include "${.CURDIR}/db/hash/Makefile.inc"

MAN3+=	db.0
