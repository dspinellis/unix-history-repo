#       @(#)Makefile	4.16 (Berkeley) %G%

SUBDIR= mailstats praliases

# don't trivially install sendmail
.if !make(install)
SUBDIR+=src
.endif

.include <bsd.subdir.mk>
