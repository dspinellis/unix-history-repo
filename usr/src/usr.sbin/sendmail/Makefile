#       @(#)Makefile	8.7 (Berkeley) %G%

SUBDIR= src mailstats makemap praliases cf/cf
FTPDIR=	mastodon:/disks/barad-dur/ftp/sendmail/.
VER=	XX
FILES=	Files.base Files.cf Files.misc Files.xdoc

tar: Files.base Files.cf Files.misc Files.xdoc
	(cd src; sccs check)
	(cd doc/op; sccs check)
	(cd doc/intro; sccs check)
	(cd doc/usenix; sccs check)
	(cd cf; sccs check)
	(cd cf/m4; sccs check)
	(cd cf/mailer; sccs check)
	(cd cf/feature; sccs check)
	(cd cf/cf; sccs check)
	(cd cf/ostype; sccs check)
	(cd cf/domain; sccs check)
	(cd src; ${MAKE})
	(cd doc; PRINTER=ps ${MAKE})
	(cd doc; chmod 444 op/op.ps intro/intro.ps usenix/usenix.ps)
	(cd cf/cf; ${MAKE})
	chmod +x src/makesendmail
	pax -w -x tar -L -f sendmail.${VER}.tar `grep -v ^# ${FILES}`
	compress sendmail.${VER}.tar

ftp: sendmail.${VER}.tar.Z
	rcp sendmail.${VER}.tar.Z RELEASE_NOTES FAQ KNOWNBUGS ${FTPDIR}

.include <bsd.subdir.mk>
