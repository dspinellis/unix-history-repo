#
# 1.2	(CWI)	87/11/18
#
#   Makefile for the Versatec Harris Emulator
#

OBS = dev.o font.o font2.o graph.o main.o parse.o put.o

#CFLAGS = -g -DDEBUGABLE
CFLAGS = -O
LIBDIR = /usr/lib

all:	ndver sort tools

install: all
	install -c ndver $(DESTDIR)${LIBDIR}/ndver80
	cd Sort; make $(MFLAGS) DESTDIR=${DESTDIR} install
	cd Tools; make $(MFLAGS) DESTDIR=${DESTDIR} install

clean:
	rm -f *.o make.out ndver
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
