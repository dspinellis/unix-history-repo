DESTDIR = 
ntroff : troff nroff term.done
troff  : 
	make -f Maketroff clean
	make -f Maketroff CC=$(CC)
nroff  :
	make -f Makenroff clean
	make -f Makenroff CC=$(CC)
term.done:
	cd term; make
	date > term.done
clean  :
	rm -f *.o term.done
install :
	install -s troff ${DESTDIR}/usr/bin
	install -s nroff ${DESTDIR}/usr/bin
	cd term; make DESTDIR=${DESTDIR} install
