#       @(#)Makefile	6.2 (Berkeley) %G%

SUBDIR= mailstats praliases

# don't trivially install sendmail
.if !make(install)
SUBDIR+=src
.endif

tar: Files
	tar chf sendmail.tar `cat Files`

.include <bsd.subdir.mk>
