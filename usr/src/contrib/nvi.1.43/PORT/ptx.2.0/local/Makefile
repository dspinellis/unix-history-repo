#	@(#)Makefile	8.1 (Berkeley) 10/9/94

# Create local patched sources.
S=	../../..
L=	bt_open.c hash.c

all:	$L

bt_open.c: ../db/btree/bt_open.c
	rm -f bt_open.c
	cp ../db/btree/bt_open.c bt_open.c
	chmod 644 bt_open.c
	patch < PATCH.bt_open.c
	chmod 444 bt_open.c
	rm -f bt_open.c.orig

hash.c: ../db/hash/hash.c
	rm -f hash.c
	cp ../db/hash/hash.c hash.c
	chmod 644 hash.c
	patch < PATCH.hash.c
	chmod 444 hash.c
	rm -f hash.c.orig

clean:
	rm -f $L
