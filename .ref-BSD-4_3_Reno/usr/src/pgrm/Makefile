#	@(#)Makefile	5.8 (Berkeley) 7/2/90
#
# remove lisp,
SUBDIR=	ar ctags error fpr fsplit gcore indent ld lex lint lorder m4 \
	mkdep mkstr nm ranlib ratfor size strings strip struct symorder \
	tsort unifdef vgrind xinstall xstr yacc

.if   ${MACHINE} == "tahoe"
SUBDIR+=as.tahoe cc cpp dbx f77 gprof pascal
.elif ${MACHINE} == "vax"
SUBDIR+=as.vax cc cpp dbx f77 gprof pascal
.endif

.include <bsd.subdir.mk>
