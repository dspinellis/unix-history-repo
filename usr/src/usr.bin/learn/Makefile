#	@(#)Makefile	4.12 (Berkeley) %G%

SUBDIR=	lcount learn learnlib lrntee objects

beforeinstall:
	(cd ${LLIB}/learn; rm -rf *)
	(cd ${LLIB}/learn; mkdir C bin editor eqn files macros morefiles)

.include "Makefile.inc"
.include <bsd.subdir.mk>
