#	@(#)Makefile	5.6 (Berkeley) %G%

SUBDIR=	compact crypt lex make yacc

.if   make(clean) || make(cleandir)
SUBDIR+=talk
.elif ${MACHINE} == "vax"
SUBDIR+=talk
.endif

.include <bsd.subdir.mk>
