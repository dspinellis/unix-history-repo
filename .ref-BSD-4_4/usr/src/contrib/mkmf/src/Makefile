#CFLAGS=	-O -DINSTALLDIR=\"$(INSTALLDIR)\"

#CFLAGS=	-O -DSYSV -DINSTALLDIR=\"$(INSTALLDIR)\" # Apollo (Domain/OS sys5)

#CFLAGS=	-O -DUNICOS5 -DINSTALLDIR=\"$(INSTALLDIR)\" # Cray Unicos 5.x

CFLAGS	      =	-O -DBSD4X -DINSTALLDIR=\"$(INSTALLDIR)\" # Generic BSD

INSTALLDIR=	/usr/contrib

PROG=		mkmf

SRCS=		Mkmf.c \
		badopt.c \
		buildlist.c \
		depend.c \
		dlist.c \
		editmf.c \
		getcwp.c \
		getproject.c \
		hash.c \
		iolin.c \
		macro.c \
		misc.c \
		mksrclist.c \
		mksymlink.c \
		mustfopen.c \
		optpath.c \
		pathcat.c \
		pathhead.c \
		pperror.c \
		rule.c \
		slappend.c \
		slinit.c \
		slrm.c \
		slsort.c \
		slvect.c \
		slvtol.c \
		strpcpy.c \
		strsav.c \
		suffix.c \
		usage.c \
		warn.c

.SUFFIXES:	.man .1

.man.1:
		sed -e s?INSTALLDIR?$(INSTALLDIR)? $< > $*.1

.include <bsd.prog.mk>
