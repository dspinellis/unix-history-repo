#       @(#)Makefile	6.3 (Berkeley) %G%

SUBDIR= mailstats praliases
VER=	XX

# don't trivially install sendmail
.if !make(install)
SUBDIR+=src
.endif

tar: Files.base Files.cf Files.misc Files.xdoc
	tar chf sendmail.${VER}.base.tar `cat Files.base`
	tar chf sendmail.${VER}.cf.tar `cat Files.cf`
	tar chf sendmail.${VER}.misc.tar `cat Files.misc`
	tar chf sendmail.${VER}.xdoc.tar `cat Files.xdoc`

.include <bsd.subdir.mk>
