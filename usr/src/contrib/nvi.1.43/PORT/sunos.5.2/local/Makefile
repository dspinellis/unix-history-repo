#	@(#)Makefile	8.1 (Berkeley) 10/12/94

# Create local patched sources.
S=	../../..
L=	v_increment.c

all:	$L

v_increment.c: $S/vi/v_increment.c
	rm -f v_increment.c
	cp $S/vi/v_increment.c v_increment.c
	chmod 644 v_increment.c
	patch < PATCH.v_increment.c
	chmod 444 v_increment.c
	rm -f v_increment.c.orig

clean:
	rm -f $L
