#	@(#)Makefile	8.2 (Berkeley) 11/10/94

# Create local patched sources.
S=	../../..
L=	svi_curses.c term.c

all:	$L

svi_curses.c: $S/svi/svi_curses.c
	rm -f svi_curses.c
	cp $S/svi/svi_curses.c svi_curses.c
	chmod 644 svi_curses.c
	patch < PATCH.svi_curses.c
	chmod 444 svi_curses.c
	rm -f svi_curses.c.orig

term.c: $S/common/term.c
	rm -f term.c
	cp $S/common/term.c term.c
	chmod 644 term.c
	patch < PATCH.term.c
	chmod 444 term.c
	rm -f term.c.orig

clean:
	rm -f $L
