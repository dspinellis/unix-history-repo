#
#   Makefile for the Versatec Harris Emulator
#

OBS = dev.o font.o font2.o graph.o main.o parse.o put.o

#CFLAGS = -g -DDEBUGABLE
CFLAGS = -O

all:	ndver sort tools

#
# only on turing do we actually need the driver.
#
install: sort
#	install -c ndver $(DESTDIR)/ndver80
	cd Sort; make $(MFLAGS) install
#	cd Tools; make $(MFLAGS) install

clean:
	rm -f *.o make.out dnver
	cd Sort; make $(MFLAGS) clean
	cd Tools; make $(MFLAGS) clean

ndver:	$(OBS)
	$(CC) $(CFLAGS) $(OBS) -lm -o ndver

sort:;  cd Sort; make $(MFLAGS) all
tools:; cd Tools; make $(MFLAGS) all


$(OBS):			Makefile
font.o put.o dev.o:	the.h dev.h
font2.o: 		the.h defs.h
main.o parse.o:		the.h
