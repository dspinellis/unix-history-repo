DESTDIR=
.f.o: ; f77 -w66 -O -c $*.f
.c.o: ; f77 -O -c $*.c
OBJECTS = \
roots.o \
acans.o \
dcops.o \
dctrans.o \
errchks.o \
ovtpvts.o \
readins.o \
setups.o \
loc.o 
spice: $(OBJECTS) ; f77 -z -o spice $(OBJECTS)
install:; install -s spice ${DESTDIR}/usr/ucb/spice
clean:; rm -f *.o spice
