DESTDIR = 
ntroff : troff nroff
troff  : 
	make -f Maketroff clean
	make -f Maketroff CC=$(CC)
nroff  :
	make -f Makenroff clean
	make -f Makenroff CC=$(CC)
clean  :
	rm -f *.o
install :
	install -s troff $(DESTDIR)/usr/bin
	install -s nroff $(DESTDIR)/usr/bin
