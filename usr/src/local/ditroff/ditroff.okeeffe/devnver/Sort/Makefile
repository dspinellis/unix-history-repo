#
#
# nvsort
#
DESTFILE=/usr/lib/nvsort80

all:	nvsort

install: nvsort
	 install -c nvsort $(DESTFILE)

clean:
	rm -f make.out nvsort

nvsort:	nvsort.c
	cc $(CFLAGS) -DVER80 -o nvsort nvsort.c -lm
