#       @(#)Makefile	6.5 (Berkeley) %G%

SUBDIR= mailstats praliases
VER=	XX

# don't trivially install sendmail
.if !make(install)
SUBDIR+=src
.endif

tar: Files.base Files.cf Files.misc Files.xdoc
	(cd src; ${MAKE})
	(cd doc; ${MAKE})
	(cd cf/cf; ${MAKE})
	tar chf sendmail.${VER}.base.tar `cat Files.base`
	compress sendmail.${VER}.base.tar
	tar chf sendmail.${VER}.cf.tar `cat Files.cf`
	compress sendmail.${VER}.cf.tar
	tar chf sendmail.${VER}.misc.tar `cat Files.misc`
	compress sendmail.${VER}.misc.tar
	tar chf sendmail.${VER}.xdoc.tar `cat Files.xdoc`
	compress sendmail.${VER}.xdoc.tar

ftp: sendmail.${VER}.base.tar.Z sendmail.${VER}.cf.tar.Z sendmail.${VER}.misc.tar.Z sendmail.${VER}.xdoc.tar.Z
	rcp sendmail.${VER}.*.tar.Z RELEASE_NOTES barad-dur:/disks/barad-dur/ftp/sendmail/.

.include <bsd.subdir.mk>
