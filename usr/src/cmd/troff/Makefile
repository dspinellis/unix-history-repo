all : troff nroff terms
troff  : 
	make -f Maketroff clean
	make -f Maketroff CC=$(CC)
nroff  :
	make -f Makenroff clean
	make -f Makenroff CC=$(CC)
terms:
	cd term; make DESTDIR=$(DESTDIR)
clean  :
	rm -f *.o
	cd term; make clean
install :
	install -s troff $(DESTDIR)/usr/bin
	install -s nroff $(DESTDIR)/usr/bin
	cd term; make install DESTDIR=$(DESTDIR)
