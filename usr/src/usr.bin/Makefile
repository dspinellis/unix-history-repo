#	@(#)Makefile	5.8.1.1 (Berkeley) %G%

# need conversion:	tn3270
# remove lisp

	window write xargs yes

.if	${MACHINE} == "hp300"
SUBDIR+=gas gcc
.elif	${MACHINE} == "i386"
SUBDIR+=gas gcc
.elif	${MACHINE} == "tahoe"
SUBDIR+=pascal
.elif	${MACHINE} == "vax"
SUBDIR+=gas pascal
.endif

.include <bsd.subdir.mk>
