#	@(#)Makefile	8.2 (Berkeley) 10/9/94

# Create local patched sources.
S=	../../..
L=	ex_tag.c

all:	$L

ex_tag.c: $S/ex/ex_tag.c
	rm -f ex_tag.c
	cp $S/ex/ex_tag.c ex_tag.c
	chmod 644 ex_tag.c
	patch < PATCH.ex_tag.c
	chmod 444 ex_tag.c
	rm -f ex_tag.c.orig

clean:
	rm -f $L
