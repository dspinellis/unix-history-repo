#	@(#)Makefile	8.3 (Berkeley) 12/2/94

# Create local patched sources.
S=	../../..
L=	bt_open.c hash.c rec_close.c

all: $L

bt_open.c: $S/PORT/db/btree/bt_open.c
	rm -f bt_open.c
	cp $S/PORT/db/btree/bt_open.c bt_open.c
	chmod 644 bt_open.c
	patch < PATCH.bt_open.c
	chmod 444 bt_open.c
	rm -f bt_open.c.orig

hash.c: $S/PORT/db/hash/hash.c
	rm -f hash.c
	cp $S/PORT/db/hash/hash.c hash.c
	chmod 644 hash.c
	patch < PATCH.hash.c
	chmod 444 hash.c
	rm -f hash.c.orig

rec_close.c: $S/PORT/db/recno/rec_close.c
	rm -f rec_close.c
	cp $S/PORT/db/recno/rec_close.c rec_close.c
	chmod 644 rec_close.c
	patch < PATCH.rec_close.c
	chmod 444 rec_close.c
	rm -f rec_close.c.orig

clean:
	rm -f $L
