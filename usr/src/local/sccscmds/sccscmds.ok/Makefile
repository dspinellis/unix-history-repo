#	@(#)Makefile	4.2	(Berkeley)	86/01/02
#
all:
	cd util; make $(MFLAGS)
	cd com; make $(MFLAGS)
	cd cmd; make $(MFLAGS)

install:
	cd cmd; make $(MFLAGS) DESTDIR=$(DESTDIR) install
	install -c sccs.hf ${DESTDIR}/usr/local/lib/sccs.hf

clean:
	cd util; make $(MFLAGS) clean
	cd com; make $(MFLAGS) clean
	cd cmd; make $(MFLAGS) clean
