DESTDIR = 
BINDIR = ${DESTDIR}/usr/new

# Warning, LIB is wired into iconc.c and icont.c and the location
# of iconx, i.e. BINDIR, is wired into icont.
# Read docs/install and config to see how to change this

LIB = ${DESTDIR}/usr/new/lib/icon

#

all :
	cd cmp/bin; make ${MFLAGS}
	cd int/bin; make ${MFLAGS}

install : all copy
	cd man; make ${MFLAGS} DESTDIR=${DESTDIR} install

copy :
	-mkdir ${LIB} ${LIB}/iconc ${LIB}/icont
	cd cmp/bin; cp pstart.o pstop.o start.o ulink utran libi.a ${LIB}/iconc
	ranlib ${LIB}/iconc/libi.a
	cd int/bin; cp ulink utran iconx.hdr ${LIB}/icont
	cd cmp/bin; install -s iconc ${BINDIR}
	cd int/bin; install -s icont ${BINDIR}; install -s iconx ${BINDIR}

clean :
	cd cmp/bin; make ${MFLAGS} clean-all clean
	cd int/bin; make ${MFLAGS} clean-all clean
