CFLAGS=-O
all: btlgrep ucbgrep

btlgrep: btlgrep.o
	cc btlgrep.o -o btlgrep

ucbgrep: ucbgrep.o
	cc ucbgrep.o -o ucbgrep

install: btlgrep ucbgrep
	strip btlgrep ucbgrep
	mv btlgrep ${DESTDIR}/bin/grep
	mv ucbgrep ${DESTDIR}/usr/ucb/grep

clean:
	rm -f *.o btlgrep ucbgrep
