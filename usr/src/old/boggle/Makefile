#	@(#)Makefile	4.2	%G%
#
DESTDIR=
CC = /bin/cc
CFLAGS = -O

all:	boggle bogdict

bogdict: comp sfile
	if [ -f /usr/local/lib/w2a ]; \
		then dict=/usr/local/lib/w2a; \
		else dict=/usr/dict/words; fi; \
	sed -f sfile $$dict | uniq | comp >bogdict

install:
	install boggle ${DESTDIR}/usr/games/boggle
	install bogdict ${DESTDIR}/usr/games/bogdict

boggle: boggle.c
	cc -o boggle ${CFLAGS} boggle.c

comp: comp.c
	cc -o comp ${CFLAGS} comp.c

clean:
	rm -f bogdict boggle comp
