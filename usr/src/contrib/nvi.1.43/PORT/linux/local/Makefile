#	@(#)Makefile	8.2 (Berkeley) 11/2/94

# Create local patched sources.
S=	../../..
L=	exf.c term.c

all:	$L

exf.c: $S/common/exf.c
	rm -f exf.c
	cp $S/common/exf.c exf.c
	chmod 644 exf.c
	patch < PATCH.exf.c
	chmod 444 exf.c
	rm -f exf.c.orig

term.c: $S/common/term.c
	rm -f term.c
	cp $S/common/term.c term.c
	chmod 644 term.c
	patch < PATCH.term.c
	chmod 444 term.c
	rm -f term.c.orig

clean:
	rm -f $L
