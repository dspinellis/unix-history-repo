/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Margo Seltzer.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)hash_func.c	8.2 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>

#include <db.h>
#include "hash.h"
#include "page.h"
#include "extern.h"

static u_int32_t hash1 __P((const void *, size_t));
static u_int32_t hash2 __P((const void *, size_t));
static u_int32_t hash3 __P((const void *, size_t));
static u_int32_t hash4 __P((const void *, size_t));

/* Global default hash function */
u_int32_t (*__default_hash) __P((const void *, size_t)) = hash4;

/*
 * HASH FUNCTIONS
 *
 * Assume that we've already split the bucket to which this key hashes,
 * calculate that bucket, and check that in fact we did already split it.
 *
 * This came from ejb's hsearch.
 */

#define PRIME1		37
#define PRIME2		1048583

static u_int32_t
hash1(keyarg, len)
	const void *keyarg;
	register size_t len;
{
	register const u_char *key;
	register u_int32_t h;

	/* Convert string to integer */
	for (key = keyarg, h = 0; len--;)
		h = h * PRIME1 ^ (*key++ - ' ');
	h %= PRIME2;
	return (h);
}

/*
 * Phong's linear congruential hash
 */
#define dcharhash(h, c)	((h) = 0x63c63cd9*(h) + 0x9c39c33d + (c))

static u_int32_t
hash2(keyarg, len)
	const void *keyarg;
	size_t len;
{
	register const u_char *e, *key;
	register u_int32_t h;
	register u_char c;

	key = keyarg;
	e = key + len;
	for (h = 0; key != e;) {
		c = *key++;
		if (!c && key > e)
			break;
		dcharhash(h, c);
	}
	return (h);
}

/*
 * This is INCREDIBLY ugly, but fast.  We break the string up into 8 byte
 * units.  On the first time through the loop we get the "leftover bytes"
 * (strlen % 8).  On every other iteration, we perform 8 HASHC's so we handle
 * all 8 bytes.  Essentially, this saves us 7 cmp & branch instructions.  If
 * this routine is heavily used enough, it's worth the ugly coding.
 *
 * OZ's original sdbm hash
 */
static u_int32_t
hash3(keyarg, len)
	const void *keyarg;
	register size_t len;
{
	register const u_char *key;
	register size_t loop;
	register u_int32_t h;

#define HASHC   h = *key++ + 65599 * h

	h = 0;
	key = keyarg;
	if (len > 0) {
		loop = (len + 8 - 1) >> 3;

		switch (len & (8 - 1)) {
		case 0:
			do {
				HASHC;
				/* FALLTHROUGH */
		case 7:
				HASHC;
				/* FALLTHROUGH */
		case 6:
				HASHC;
				/* FALLTHROUGH */
		case 5:
				HASHC;
				/* FALLTHROUGH */
		case 4:
				HASHC;
				/* FALLTHROUGH */
		case 3:
				HASHC;
				/* FALLTHROUGH */
		case 2:
				HASHC;
				/* FALLTHROUGH */
		case 1:
				HASHC;
			} while (--loop);
		}
	}
	return (h);
}

/* Hash function from Chris Torek. */
static u_int32_t
hash4(keyarg, len)
	const void *keyarg;
	register size_t len;
{
	register const u_char *key;
	register size_t loop;
	register u_int32_t h;

#define HASH4a   h = (h << 5) - h + *key++;
#define HASH4b   h = (h << 5) + h + *key++;
#define HASH4 HASH4b

	h = 0;
	key = keyarg;
	if (len > 0) {
		loop = (len + 8 - 1) >> 3;

		switch (len & (8 - 1)) {
		case 0:
			do {
				HASH4;
				/* FALLTHROUGH */
		case 7:
				HASH4;
				/* FALLTHROUGH */
		case 6:
				HASH4;
				/* FALLTHROUGH */
		case 5:
				HASH4;
				/* FALLTHROUGH */
		case 4:
				HASH4;
				/* FALLTHROUGH */
		case 3:
				HASH4;
				/* FALLTHROUGH */
		case 2:
				HASH4;
				/* FALLTHROUGH */
		case 1:
				HASH4;
			} while (--loop);
		}
	}
	return (h);
}
