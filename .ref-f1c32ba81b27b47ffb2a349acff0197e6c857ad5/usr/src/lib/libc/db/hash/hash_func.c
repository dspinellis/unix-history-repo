/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Margo Seltzer.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)hash_func.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

/* Global default hash function */
static	int	hash1();
static	int	hash2();
static	int	hash3();
static	int	hash4();

int	(*default_hash)() = hash4;

/******************************* HASH FUNCTIONS **************************/
/*
	Assume that we've already split the bucket to which this
	key hashes, calculate that bucket, and check that in fact
	we did already split it.

	This came from ejb's hsearch.
*/

# define PRIME1			37
# define PRIME2			1048583

static int
hash1(key,len)
char *key;
int len;
{
	register int h;
	register int l = len;
	register unsigned char *k = (unsigned char *) key;

	h = 0;
	/*
	 * Convert string to integer
	 */
	while (l--) h = h * PRIME1 ^ (*k++ - ' ');
	h %= PRIME2;

	return (h);
}

/*
    Phong's linear congruential hash
*/
#define dcharhash(h, c)	((h) = 0x63c63cd9*(h) + 0x9c39c33d + (c))

static int
hash2(str, n)
	register unsigned char *str;
	int n;
{
	register unsigned char *e, c;
	register int h;

	e = str + n;
	for (h = 0; str != e;) {
		c = *str++;
		if (!c && str > e)
			break;
		dcharhash(h,c);
	}
	return(h);
}

/*
 * This is INCREDIBLY ugly, but fast.
 * We break the string up into 8 byte units.  On the first time
 * through the loop we get the "leftover bytes" (strlen % 8).
 * On every other iteration, we perform 8 HASHC's so we handle
 * all 8 bytes.  Essentially, this saves us 7 cmp & branch
 * instructions.  If this routine is heavily used enough, it's
 * worth the ugly coding
 * 
 * OZ's original sdbm hash
 */
static int
hash3(key,nbytes)
char *key;
int nbytes;
{
        register int n = 0;
	register char *str = key;
	register int loop;
	register int len = nbytes;

#define HASHC   n = *str++ + 65599 * n

        if (len > 0) {
                loop = (len + 8 - 1) >> 3;

                switch(len & (8 - 1)) {
			case 0: do {		/* All fall throughs */
					HASHC;  
				case 7: HASHC;
				case 6: HASHC;  
				case 5: HASHC;
				case 4: HASHC;  
				case 3: HASHC;
				case 2: HASHC;  
				case 1: HASHC;
                        } while (--loop);
                }

        }
	return(n);
}

/* Hash function from Chris Torek */
static int
hash4(key,nbytes)
char	*key;
int	nbytes;
{
        register int h = 0;
	register char *p = key;
	register int loop;
	register int len = nbytes;

#define HASH4a   h = (h << 5) - h + *p++;
#define HASH4b   h = (h << 5) + h + *p++;
#define HASH4 HASH4b

        if (len > 0) {
                loop = (len + 8 - 1) >> 3;

                switch(len & (8 - 1)) {
			case 0: do {		/* All fall throughs */
					HASH4;  
				case 7: HASH4;
				case 6: HASH4;  
				case 5: HASH4;
				case 4: HASH4;  
				case 3: HASH4;
				case 2: HASH4;  
				case 1: HASH4;
                        } while (--loop);
                }

        }
	return(h);
}
