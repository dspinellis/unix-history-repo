#
#  1.2	(CWI)	87/04/01
#
# nvsort
#
DESTDIR =
LIBDIR=/usr/lib

all:	nvsort

install: nvsort
	 install -c nvsort $(DESTDIR)${LIBDIR}/nvsort

clean:
	rm -f make.out nvsort

nvsort:	nvsort.c
	cc $(CFLAGS) -DVER80 -o nvsort nvsort.c -lm
