#	@(#)Makefile	8.1 (Berkeley) 10/9/94

# Create local patched sources.
S=	../../..
L=	recover.c

all:	$L

recover.c: $S/common/recover.c
	rm -f recover.c
	cp $S/common/recover.c recover.c
	chmod 644 recover.c
	patch < PATCH.recover.c
	chmod 444 recover.c
	rm -f recover.c.orig

clean:
	rm -f $L
