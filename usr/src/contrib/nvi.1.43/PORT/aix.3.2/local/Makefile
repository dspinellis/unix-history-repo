#	@(#)Makefile	8.3 (Berkeley) 10/29/94

# Create local patched sources.
S=	../../..
L=	ex_tag.c sex_refresh.c

all: $L

ex_tag.c: $S/ex/ex_tag.c
	rm -f ex_tag.c
	cp $S/ex/ex_tag.c ex_tag.c
	chmod 644 ex_tag.c
	patch < PATCH.ex_tag.c
	chmod 444 ex_tag.c
	rm -f ex_tag.c.orig

sex_refresh.c: $S/sex/sex_refresh.c
	rm -f sex_refresh.c
	cp $S/sex/sex_refresh.c sex_refresh.c
	chmod 644 sex_refresh.c
	patch < PATCH.sex_refresh.c
	chmod 444 sex_refresh.c
	rm -f sex_refresh.c.orig

clean:
	rm -f $L
