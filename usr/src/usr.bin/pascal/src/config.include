#	@(#)config.include	8.1 (Berkeley) 6/6/93

VERSION=	3.1
DATE=		6/6/93
LIBEXEC=	/usr/libexec/pascal
LIBDATA=	/usr/libdata/pascal
INSTALLDIR=	/usr/bin
ERRORSTRINGS=	${WHOAMI}${VERSION}strings
HOWFILE=	how_${WHOAMI}

config.c: ${SRCDIR}/CONFIG.c
	sed -e "s?VERSION?${VERSION}?g" \
	    -e "s?DATE?${DATE}?g" \
	    -e "s?INSTALLDIR?${INSTALLDIR}?g" \
	    -e "s?LIBEXEC?${LIBEXEC}?g" \
	    -e "s?LIBDATA?${LIBDATA}?g" \
	    -e "s?ERRORSTRINGS?${ERRORSTRINGS}?g" \
	    -e "s?HOWFILE?${HOWFILE}?g" \
	    < ${SRCDIR}/CONFIG.c > ${.TARGET}
