# Psuedo makefile for Ingres.
#
# NOTE: This is NOT the "real" makefile, the true makefile
# can be found in source/conf/Makefile. This just allows the
# "make clean", "make DESTDIR=/whatever install" to work.

binaries:
	cd source/conf ; make allinstall

clean:
	cd source/conf ; make allclean

install: binaries
	-mkdir $(DESTDIR)/bin/ingres
	-rcp -r bin $(DESTDIR)/bin/ingres
	-rcp -r files $(DESTDIR)/bin/ingres

