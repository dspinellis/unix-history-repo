#	@(#)Makefile	5.7 (Berkeley) %G%

# need conversion:	tn3270
# remove ld, lisp

	window write xargs yes

.if	${MACHINE} == "hp300"
SUBDIR+=gas gcc
.elif	${MACHINE} == "i386"
SUBDIR+=gas gcc
.elif	${MACHINE} == "tahoe"
SUBDIR+=f77 pascal systat
.elif	${MACHINE} == "vax"
SUBDIR+=f77 gas pascal systat
.endif

.include <bsd.subdir.mk>
