#
#	@(#)makefile	1.2 8/5/82
#
SRCS = Maketroff Makenroff
DESTDIR = 
ntroff : troff nroff term.done
troff  : Maketroff
	make -f Maketroff clean
	make -f Maketroff sources
	make -f Maketroff CC=$(CC)
nroff  : Makenroff
	make -f Makenroff clean
	make -f Makenroff sources
	make -f Makenroff CC=$(CC)
term.done:
	cd term; make all
	date > term.done
clean  :
	rm -f *.o term.done
install :
	install -s troff ${DESTDIR}/usr/bin
	install -s nroff ${DESTDIR}/usr/bin
	cd term; make DESTDIR=${DESTDIR} install
sources: $(SRCS)

$(SRCS):
	sccs get $@
