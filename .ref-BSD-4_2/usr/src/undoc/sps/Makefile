CFLAGS = -g -DVMUNIX
INCLUDE = /ra/sys
all:	sps

sps:	sps.o
	cc $(CFLAGS) -o sps sps.o -ljobs

sps.o:	sps.c
	cc -c $(CFLAGS) -I$(INCLUDE) sps.c

install:	sps
		cp sps /usr/local
		strip /usr/local/sps
		chmod 4711 /usr/local/sps
