#       @(#)Makefile	4.16 (Berkeley) 5/11/90

SUBDIR= mailstats # praliases

# don't trivially install sendmail
.if !make(install)
SUBDIR+=src
.endif

.include <bsd.subdir.mk>
