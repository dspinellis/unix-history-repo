#
#  1.3	(CWI)	87/07/16
#
# nvsort
#
DESTDIR =
LIBDIR=/usr/lib

all:	nvsort

install: nvsort
	 install -c nvsort $(DESTDIR)${LIBDIR}/nvsort80

clean:
	rm -f make.out nvsort

nvsort:	nvsort.c
	cc $(CFLAGS) -DVER80 -o nvsort nvsort.c -lm
