/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Margo Seltzer.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)page.c	5.13 (Berkeley) 6/17/91";
#endif /* LIBC_SCCS and not lint */

/******************************************************************************
PACKAGE:  hashing

DESCRIPTION: 
	Page manipulation for hashing package.

ROUTINES: 
    External
	__get_page
	__add_ovflpage
    Internal
	overflow_page
	open_temp
******************************************************************************/

#include <sys/param.h>
#include <fcntl.h>
#include <signal.h>
#include <assert.h>
#include <errno.h>
#include <db.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "hash.h"
#include "page.h"

/* Externals */
/* buf.c */
extern BUFHEAD	*__get_buf();
extern void __reclaim_buf();

/* big.c */
extern int __big_split();
extern int __big_insert();
extern int __big_delete();
extern int __find_bigpair();

/* dynahash.c */
extern	u_int	__call_hash();
extern	int	__expand_table();

/* my externals */
extern int __get_page();
extern BUFHEAD *__add_ovflpage();
extern int __split_page();
extern int __addel();

/* my internals */
static u_short overflow_page();
static int open_temp();
static int ugly_split();
static void squeeze_key();
static void putpair();
static u_long *fetch_bitmap();

#ifdef HASH_STATISTICS
extern long hash_accesses, hash_collisions, hash_expansions, hash_overflows;
#endif
#define	PAGE_INIT(P)					\
{							\
    ((u_short *)P)[0] = 0;				\
    ((u_short *)P)[1] = hashp->BSIZE - 3 * sizeof(u_short);	\
    ((u_short *)P)[2] = hashp->BSIZE;			\
}

/*
	This is called AFTER we have verified that there is room on the
	page for the pair (PAIRFITS has returned true) so we go right
	ahead and start moving stuff on.
*/
static void
putpair(p, key, val)
char *p;
DBT *key;
DBT *val;
{
	register u_short n;
	register u_short off;
	register u_short *bp = (u_short *) p;

/* enter the key first */
	n = bp[0];

	off = OFFSET(bp) - key->size;
	bcopy( key->data, p+off, key->size );
	bp[++n] = off;

/* now the data */
	off -= val->size;
	bcopy(val->data,  p + off, val->size);
	bp[++n] = off;

/* adjust page info */
	bp[0] = n;
	bp[n+1] = off - ((n+3)*sizeof(u_short));
	bp[n+2] = off;
	return;
}
/*
    0 OK
    -1 error
*/
extern int
__delpair(bufp, ndx)
BUFHEAD *bufp;
register int ndx;
{
	register u_short *bp = (u_short *) bufp->page;
	register int n = bp[0];
	register u_short newoff;
	u_short pairlen;

	if ( bp[ndx+1] < REAL_KEY ) return ( __big_delete ( bufp, ndx ) );
	if ( ndx != 1 ) newoff = bp[ndx-1];
	else newoff = hashp->BSIZE;
	pairlen = newoff - bp[ndx+1];

	if ( ndx != (n-1) ) {
		/* Hard Case -- need to shuffle keys */
		register int i;
		register char *src = bufp->page + (int)OFFSET(bp);
		register char *dst = src + (int)pairlen;
		bcopy ( src, dst, bp[ndx+1] - OFFSET(bp) );

		/* Now adjust the pointers */
		for ( i = ndx+2; i <= n; i += 2 ) {
		    if ( bp[i+1]  == OVFLPAGE ) {
			bp[i-2] = bp[i];
			bp[i-1] = bp[i+1];
		    } else {
			bp[i-2] = bp[i] + pairlen;
			bp[i-1] = bp[i+1] + pairlen;
		    }
		}
	}

	/* Finally adjust the page data */
	bp[n] = OFFSET(bp) + pairlen;
	bp[n-1] = bp[n+1] + pairlen + 2 * sizeof(u_short);
	bp[0] = n-2;
	hashp->NKEYS--;

	bufp->flags |= BUF_MOD;
	return (0);
}
/*
    -1 ==> Error
    0 ==> OK
*/
extern int
__split_page(obucket, nbucket)
u_int obucket;
u_int nbucket;
{
	DBT key;
	DBT val;

	register BUFHEAD *new_bufp;
	register BUFHEAD *old_bufp;
	register u_short *ino;
	register char	*np;
	int n;
	int ndx;
	int retval;
	char	*op;

	u_short copyto = (u_short)hashp->BSIZE;
	u_short diff;
	u_short off = (u_short)hashp->BSIZE;
	u_short moved;

	old_bufp = __get_buf ( obucket, NULL, 0 );
	new_bufp = __get_buf ( nbucket, NULL, 0 );

	old_bufp->flags |= (BUF_MOD|BUF_PIN);
	new_bufp->flags |= (BUF_MOD|BUF_PIN);

	ino = (u_short *)(op = old_bufp->page);
	np = new_bufp->page;

	moved = 0;

	for (n = 1, ndx = 1; n < ino[0]; n+=2) {
		if ( ino[n+1] < REAL_KEY ) {
		    retval = ugly_split( obucket, old_bufp, new_bufp, 
					 copyto, moved );
		    old_bufp->flags &= ~BUF_PIN;
		    new_bufp->flags &= ~BUF_PIN;
		    return(retval);
		    
		}
		key.data = (u_char *)op + ino[n]; 
		key.size = off - ino[n];

		if ( __call_hash ( key.data, key.size ) == obucket ) {
		    /* Don't switch page */
		    diff = copyto - off;
		    if ( diff ) {
			copyto = ino[n+1] + diff;
			bcopy ( op + ino[n+1], op + copyto,  off-ino[n+1]);
			ino[ndx] = copyto + ino[n] - ino[n+1];
			ino[ndx+1] = copyto;
		    } else copyto = ino[n+1];
		    ndx += 2;
		} else {
		    /* Switch page */
		    val.data = (u_char *)op + ino[n+1];
		    val.size = ino[n] - ino[n+1];
		    putpair( np, &key, &val);
		    moved +=2;
		}

		off = ino[n+1];
	}

	/* Now clean up the page */
	ino[0] -= moved;
	FREESPACE(ino) = copyto - sizeof(u_short) * (ino[0]+3);
	OFFSET(ino) = copyto;

#ifdef DEBUG3
	fprintf(stderr, "split %d/%d\n", 
	       ((u_short *) np)[0] / 2,
	       ((u_short *) op)[0] / 2);
#endif
	/* unpin both pages */
	old_bufp->flags &= ~BUF_PIN;
	new_bufp->flags &= ~BUF_PIN;
	return(0);
}
/*
    0 ==> success
    -1 ==> failure

    Called when we encounter an overflow or big key/data page during 
    split handling.
    This is special cased since we have to begin checking whether
    the key/data pairs fit on their respective pages and because
    we may need overflow pages for both the old and new pages

    The first page might be a page with regular key/data pairs
    in which case we have a regular overflow condition and just
    need to go on to the next page or it might be a big key/data 
    pair in which case we need to fix the big key/data pair.
*/
static int
ugly_split( obucket, old_bufp, new_bufp, copyto, moved )
u_int	obucket;	/* Same as __split_page */
BUFHEAD	*old_bufp;		
BUFHEAD	*new_bufp;
u_short	copyto;		/* First byte on page which contains key/data values */
int	moved;		/* number of pairs moved to new page */
{
    register BUFHEAD *bufp = old_bufp;		/* Buffer header for ino */
    register u_short	*ino = (u_short *)old_bufp->page;	
						/* Page keys come off of */
    register u_short	*np = (u_short *)new_bufp->page;	/* New page */
    register u_short	*op = (u_short *)old_bufp->page;	
						/* Page keys go on to if they
						   aren't moving */

    char	*cino;				/* Character value of ino */
    BUFHEAD	*last_bfp = NULL;		/* Last buffer header OVFL which
						   needs to be freed */
    u_short	ov_addr, last_addr = 0;
    u_short	n;
    u_short	off;

    DBT	key, val;
    SPLIT_RETURN	ret;

    n = ino[0]-1;
    while ( n < ino[0] ) {
	if ( ino[2] < REAL_KEY && ino[2] != OVFLPAGE ) {
	    if (__big_split (old_bufp, new_bufp, bufp, ov_addr, obucket, &ret)) {
		return(-1);
	    }
	    old_bufp = ret.oldp;
	    if ( !old_bufp ) return(-1);
	    op = (u_short *)old_bufp->page;
	    new_bufp = ret.newp;
	    if ( !new_bufp ) return(-1);
	    np = (u_short *)new_bufp->page;
	    bufp = ret.nextp;
	    if ( !bufp ) return(0);
	    cino = (char *)bufp->page;
	    ino = (u_short *)cino;
	    last_bfp = ret.nextp;
	} else if ( ino[n+1] == OVFLPAGE ) {
	    ov_addr = ino[n];
	    /* 
		Fix up the old page -- the extra 2 are the fields which
		contained the overflow information 
	    */
	    ino[0] -= (moved + 2);
	    FREESPACE(ino) = copyto - sizeof(u_short) * (ino[0]+3);
	    OFFSET(ino) = copyto;

	    bufp = __get_buf ( ov_addr, bufp, 0 );
	    if ( !bufp ) return(-1);

	    ino = (u_short *)bufp->page;
	    n = 1;
	    copyto = hashp->BSIZE;
	    moved = 0;

	    if ( last_bfp ) {
		__free_ovflpage( last_bfp);
	    }
	    last_bfp = bufp;
	} 
	

	/* Move regular sized pairs of there are any */
	off = hashp->BSIZE;
	for ( n = 1; (n < ino[0]) && (ino[n+1] >= REAL_KEY); n += 2 ) {
	    cino = (char *)ino;
	    key.data = (u_char *)cino + ino[n]; 
	    key.size = off - ino[n];
	    val.data = (u_char *)cino + ino[n+1];
	    val.size = ino[n] - ino[n+1];
	    off = ino[n+1];

	    if ( __call_hash ( key.data, key.size ) == obucket ) {
		/* Keep on old page */
		if (PAIRFITS(op,(&key),(&val))) putpair((char *)op, &key, &val);
		else {
		    old_bufp = __add_ovflpage ( old_bufp );
		    if ( !old_bufp ) return(-1);
		    op = (u_short *)old_bufp->page;
		    putpair ((char *)op, &key, &val);
		}
		old_bufp->flags |= BUF_MOD;
	    } else {
		/* Move to new page */
		if (PAIRFITS(np,(&key),(&val))) putpair((char *)np, &key, &val);
		else {
		    new_bufp = __add_ovflpage ( new_bufp );
		    if ( !new_bufp )return(-1);
		    np = (u_short *)new_bufp->page;
		    putpair ((char *)np, &key, &val);
		} 
		new_bufp->flags |= BUF_MOD;
	    }
	}
    }
    if ( last_bfp ) {
	__free_ovflpage(last_bfp);
    }

    return (0);
}
/*
    Add the given pair to the page
    1 ==> failure
    0 ==> OK
*/
extern int
__addel(bufp, key, val)
BUFHEAD	*bufp;
DBT	*key;
DBT	*val;
{
    register u_short *bp = (u_short *)bufp->page;
    register u_short *sop;
    int	do_expand;

    do_expand = 0;
    while ( bp[0] &&  (bp[bp[0]] < REAL_KEY) ) {
	/* Exception case */
	if ( bp[2] < REAL_KEY ) {
	    /* This is a big-keydata pair */
	    bufp = __add_ovflpage(bufp);
	    if ( !bufp ) {
		return(-1);
	    }
	    bp = (u_short *)bufp->page;
	} else {
	    /* Try to squeeze key on this page */
	    if ( FREESPACE(bp) > PAIRSIZE(key,val) ) {
		squeeze_key ( bp, key, val );
		return(0);
	    } else {
		bufp = __get_buf ( bp[bp[0]-1], bufp, 0 );
		if (!bufp) {
		    return(-1);
		}
		bp = (u_short *)bufp->page;
	    }
	}
    }

    if ( PAIRFITS(bp,key,val) ) putpair (bufp->page, key, val);
    else {
	do_expand = 1;
	bufp = __add_ovflpage ( bufp );
	if (!bufp)return(-1);
	sop = (u_short *) bufp->page;

	if ( PAIRFITS(sop, key, val) ) putpair ( (char *)sop, key, val );
	else if ( __big_insert ( bufp, key, val ) ) {
	    return(-1);
	}
    }
    bufp->flags |= BUF_MOD;
    /* 
	If the average number of keys per bucket exceeds the fill factor,
	expand the table
    */
    hashp->NKEYS++;
    if (do_expand || 
	(hashp->NKEYS / (hashp->MAX_BUCKET+1) > hashp->FFACTOR) ) {
	 return(__expand_table());
    }
    return(0);
}

/*
    returns a pointer, NULL on error
*/
extern BUFHEAD *
__add_ovflpage ( bufp )
BUFHEAD	*bufp;
{
    register	u_short	*sp = (u_short *)bufp->page;

    u_short	ovfl_num;
    u_short	ndx, newoff;
    char	*op;
    DBT	okey, oval;
#ifdef DEBUG1
    int	tmp1, tmp2;
#endif

    bufp->flags |= BUF_MOD;
    ovfl_num = overflow_page ();
#ifdef DEBUG1
    tmp1 = bufp->addr;
    tmp2 = bufp->ovfl?bufp->ovfl->addr:0;
#endif
    if (!ovfl_num || !(bufp->ovfl = __get_buf ( ovfl_num, bufp, 1 ))) {
	return(NULL);
    }
    bufp->ovfl->flags |= BUF_MOD;
#ifdef DEBUG1
    fprintf ( stderr, "ADDOVFLPAGE: %d->ovfl was %d is now %d\n", tmp1, tmp2, 
		bufp->ovfl->addr );
#endif
    ndx = sp[0];
    /* 
	Since a pair is allocated on a page only if there's room
	to add an overflow page, we know that the OVFL information
	will fit on the page
    */
    sp[ndx+4] = OFFSET(sp);
    sp[ndx+3] = FREESPACE(sp) - OVFLSIZE;
    sp[ndx+1] = ovfl_num;
    sp[ndx+2] = OVFLPAGE;
    sp[0] = ndx+2;
#ifdef HASH_STATISTICS
	    hash_overflows++;
#endif
    return(bufp->ovfl);
}

/*
    0 indicates SUCCESS
    -1 indicates FAILURE
*/
extern	int
__get_page ( p, bucket, is_bucket, is_disk, is_bitmap )
char	*p;
u_int	bucket;
int	is_bucket;
int	is_disk;
int	is_bitmap;
{
    register int size;
    register int fd;
    register int page;
    u_short	*bp;
    int		rsize;

    fd = hashp->fp;
    size = hashp->BSIZE;

    if ( (fd == -1) || !is_disk ) { 
	PAGE_INIT(p);
	return(0);
    }

    if ( is_bucket) page = BUCKET_TO_PAGE (bucket);
    else page = OADDR_TO_PAGE (bucket);
    if ((lseek ( fd, page << hashp->BSHIFT, SEEK_SET ) == -1) || 
	((rsize = read ( fd, p, size )) == -1 )) {
	return(-1);
    } 
    bp = (u_short *)p;
    if ( !rsize ) {
	bp[0] = 0;		/* We hit the EOF, so initialize a new page */
    } else if ( rsize != size ) {
	errno = EFTYPE;		
	return(-1);
    }
    if (!bp[0]) {
	PAGE_INIT(p);
    } else if ( hashp->LORDER != BYTE_ORDER ) {
	register int i;
	register int max;

	if ( is_bitmap ) {
	    max = hashp->BSIZE >> 2;	/* divide by 4 */
	    for ( i=0; i < max; i++ ) {
		BLSWAP(((long *)p)[i]);
	    }
	} else {
	    BSSWAP(bp[0]);
	    max = bp[0] + 2;
	    for ( i=1; i <= max; i++ ) {
		BSSWAP(bp[i]);
	    }
	}
    }
    return (0);
}

/* 
    Write page p to disk
    -1==>failure
    0==> OK
*/
extern int
__put_page ( p, bucket, is_bucket, is_bitmap )
char	*p;
u_int	bucket;
int	is_bucket;
int	is_bitmap;
{
    register int size;
    register int fd;
    register int page;
    int	wsize;

    size = hashp->BSIZE;
    if ( (hashp->fp == -1) && open_temp() ) return (1);
    fd = hashp->fp;

    if ( hashp->LORDER != BYTE_ORDER ) {
	register int i;
	register int max;

	if ( is_bitmap ) {
	    max = hashp->BSIZE >> 2;	/* divide by 4 */
	    for ( i=0; i < max; i++ ) {
		BLSWAP(((long *)p)[i]);
	    }
	} else {
	    max = ((u_short *)p)[0] + 2;
	    for ( i=0; i <= max; i++ ) {
		BSSWAP(((u_short *)p)[i]);
	    }
	}
    }
    if (is_bucket ) page = BUCKET_TO_PAGE (bucket);
    else page = OADDR_TO_PAGE ( bucket );
    if ((lseek ( fd, page << hashp->BSHIFT, SEEK_SET ) == -1) || 
	((wsize = write ( fd, p, size )) == -1 )) {
	/* Errno is set */
	return(-1);
    }
    if ( wsize != size ) {
	errno = EFTYPE;	
	return(-1);
    }
    return(0);
}
#define BYTE_MASK	((1 << INT_BYTE_SHIFT) -1)
/*
    Initialize a new bitmap page.  Bitmap pages are left in memory
    once they are read in.
*/
extern u_long *
__init_bitmap(pnum, nbits, ndx)
u_short	pnum;
int	nbits;
int	ndx;
{
    u_long	*ip;
    int		clearints;
    int		clearbytes;

    if ( !(ip = (u_long *)malloc (hashp->BSIZE)) ) return (NULL);
    hashp->nmaps++;
    clearints = ((nbits - 1) >> INT_BYTE_SHIFT) + 1;
    clearbytes = clearints << INT_TO_BYTE;
    memset ((char *)ip, 0, clearbytes );
    memset ( ((char *) ip) + clearbytes, 0xFF, 
		hashp->BSIZE-clearbytes );
    ip[clearints-1] = ALL_SET << (nbits & BYTE_MASK);
    SETBIT(ip, 0);
    hashp->BITMAPS[ndx] = pnum;
    hashp->mapp[ndx] = ip;
    return(ip);
}
static int
first_free ( map )
u_long map;
{
    register u_long      mask;
    register u_long      i;

    mask = 0x1;
    for ( i=0; i < BITS_PER_MAP; i++ ) {
	if ( !(mask & map) ) return(i);
	mask = mask << 1;
    }
    return ( i );
}

static u_short
overflow_page ( )
{
    register	int max_free;
    register	int splitnum;
    register	u_long *freep;
    register	int offset;
    u_short	addr;
    int		in_use_bits;
    int		free_page, free_bit;
    int		i, j, bit;
#ifdef DEBUG2
    int	tmp1, tmp2;
#endif

    splitnum = __log2(hashp->MAX_BUCKET);
    max_free = hashp->SPARES[splitnum];

    free_page = (max_free-1) >> (hashp->BSHIFT + BYTE_SHIFT);
    free_bit  = (max_free-1) & ((hashp->BSIZE << BYTE_SHIFT) - 1);

    /* Look through all the free maps to find the first free block */
    for ( i = 0; i <= free_page; i++ ) {
	if (!(freep = (u_long *)hashp->mapp[i])  &&
	    !(freep = fetch_bitmap(i)) ) {
	    return ( NULL );
	}
	if ( i == free_page ) in_use_bits = free_bit;
	else in_use_bits = (hashp->BSIZE << BYTE_SHIFT) -1;

	for (j = 0, bit = 0; bit <= in_use_bits; j++, bit += BITS_PER_MAP ) {
	     if ( freep[j] != ALL_SET ) goto found;
	}
    }
    /* No Free Page Found */
    hashp->SPARES[splitnum]++;
    offset = hashp->SPARES[splitnum] - 
	     (splitnum ? hashp->SPARES[splitnum-1] : 0);

    /* Check if we need to allocate a new bitmap page */
    if ( free_bit == (hashp->BSIZE << BYTE_SHIFT) - 1 ) {
	free_page++;
#define	OVMSG	"hash: out of overflow pages; increase page size\n"
	if ( free_page >= NCACHED ) {
	    (void) write (STDERR_FILENO, OVMSG, sizeof(OVMSG) - 1);
	    return(NULL);
	}
	/* 
	    This is tricky.  The 1 indicates that you want the
	    new page allocated with 1 clear bit.  Actually, you
	    are going to allocate 2 pages from this map.  The first
	    is going to be the map page, the second is the overflow
	    page we were looking for.  The init_bitmap routine
	    automatically, sets the first bit of itself to indicate
	    that the bitmap itself is in use.  We would explicitly
	    set the second bit, but don't have to if we tell init_bitmap
	    not to leave it clear in the first place.
	*/
	__init_bitmap ( OADDR_OF(splitnum, offset), 1, free_page );
	hashp->SPARES[splitnum]++;
#ifdef DEBUG2
	free_bit = 2;
#endif
	offset++;
    } else {
	/* 
		Free_bit addresses the last used bit.  Bump it to
		address the first available bit.
	*/	
	free_bit++;
	SETBIT ( freep, free_bit );
    }

    /* Calculate address of the new overflow page */
    if ( offset > SPLITMASK ) {
	(void) write (STDERR_FILENO, OVMSG, sizeof(OVMSG) - 1);
	return(NULL);
    }
    addr = OADDR_OF(splitnum, offset);
#ifdef DEBUG2
    fprintf ( stderr, "OVERFLOW_PAGE: ADDR: %d BIT: %d PAGE %d\n",
		addr, free_bit, free_page );
#endif
    return(addr);

found:
    bit = bit + first_free(freep[j]);
    SETBIT(freep,bit);
#ifdef DEBUG2
    tmp1 = bit;
    tmp2 = i;
#endif
    /* 
	Bits are addressed starting with 0, but overflow pages are
	addressed beginning at 1. Bit is a bit addressnumber, so we 
	need to increment it to convert it to a page number.
    */
    bit = 1 + bit + (i * (hashp->BSIZE << BYTE_SHIFT));

    /* Calculate the split number for this page */
    for ( i = 0; (i < splitnum) && (bit > hashp->SPARES[i]); i++ );
    offset =(i ? bit - hashp->SPARES[i-1] : bit );
    if ( offset >= SPLITMASK ) return(NULL);/* Out of overflow pages */
    addr = OADDR_OF(i, offset);
#ifdef DEBUG2
    fprintf ( stderr, "OVERFLOW_PAGE: ADDR: %d BIT: %d PAGE %d\n",
		addr, tmp1, tmp2 );
#endif

    /* Allocate and return the overflow page */
    return (addr);
}

/*
    Mark this overflow page as free.
*/
__free_ovflpage ( obufp )
BUFHEAD	*obufp;
{
    register u_short addr = obufp->addr;
    int	free_page, free_bit;
    int bit_address;
    u_short ndx;
    u_long *freep;
    int j;

#ifdef DEBUG1
    fprintf ( stderr, "Freeing %d\n", addr );
#endif
    ndx = (((u_short)addr) >> SPLITSHIFT);
    bit_address = (ndx ? hashp->SPARES[ndx-1] : 0) + (addr & SPLITMASK) - 1;
    free_page = (bit_address >> (hashp->BSHIFT + BYTE_SHIFT));
    free_bit  = bit_address & ((hashp->BSIZE << BYTE_SHIFT) - 1);

    if ( !(freep = hashp->mapp[free_page]) &&
	 !(freep = fetch_bitmap( free_page )) ) {
	/* 
	    This had better never happen.  It means we tried to
	    read a bitmap that has already had overflow pages allocated
	    off it, and we failed to read it from the file
	*/
	assert(0);
    }
    CLRBIT(freep, free_bit);
#ifdef DEBUG2
    fprintf ( stderr, "FREE_OVFLPAGE: ADDR: %d BIT: %d PAGE %d\n",
		obufp->addr, free_bit, free_page );
#endif
    __reclaim_buf ( obufp );
    return;
}

/*
0 success
-1 failure
*/
static int
open_temp()
{
    sigset_t	set, oset;
    static char	namestr[] = "_hashXXXXXX";

    /* Block signals; make sure file goes away at process exit. */
    sigemptyset(&set);
    sigaddset(&set, SIGHUP);
    sigaddset(&set, SIGINT);
    sigaddset(&set, SIGQUIT);
    sigaddset(&set, SIGTERM);
    (void)sigprocmask(SIG_BLOCK, &set, &oset);
    if ((hashp->fp = mkstemp ( namestr )) != -1) {
	(void)unlink(namestr);
	(void)fcntl(hashp->fp, F_SETFD, 1);
    }
    (void)sigprocmask(SIG_SETMASK, &oset, (sigset_t *)NULL);
    return(hashp->fp != -1 ? 0 : -1);
}

/* 
    We have to know that the key will fit, but the
    last entry on the page is an overflow pair, so we
    need to shift things.
*/
static void
squeeze_key ( sp, key, val )
u_short	*sp;
DBT	*key;
DBT	*val;
{
    register	char	*p = (char *)sp;
    u_short	free_space, off;
    u_short	pageno, n;

    n = sp[0];
    free_space = FREESPACE(sp);
    off = OFFSET(sp);

    pageno = sp[n-1];
    off -= key->size;
    sp[n-1] = off;
    bcopy ( key->data, p + off, key->size );
    off -= val->size;
    sp[n] = off;
    bcopy ( val->data, p + off, val->size );
    sp[0] = n+2;
    sp[n+1] = pageno;
    sp[n+2] = OVFLPAGE;
    FREESPACE(sp) = free_space - PAIRSIZE(key,val);
    OFFSET(sp) = off;
}

static u_long *
fetch_bitmap ( ndx ) 
int	ndx;
{
    if ( ndx >= hashp->nmaps  ||
	!(hashp->mapp[ndx] = (u_long *)malloc ( hashp->BSIZE )) ||
	 __get_page ((char *)hashp->mapp[ndx], hashp->BITMAPS[ndx], 0, 1, 1)) {

	    return(NULL);
    }
    return ( hashp->mapp[ndx] );
}
#ifdef DEBUG4
print_chain ( addr )
short	addr;
{
	BUFHEAD	*bufp;
	short	*bp;
	short	oaddr;

	fprintf ( stderr, "%d ", addr );
	bufp = __get_buf ( (int)addr, NULL, 0 );
	bp = (short *)bufp->page;
	while ( bp[0] && 
		((bp[bp[0]] == OVFLPAGE) ||
		 ((bp[0] > 2) && bp[2] < REAL_KEY))) {
		oaddr = bp[bp[0]-1];
		fprintf ( stderr, "%d ", (int)oaddr );
		bufp = __get_buf ( (int)oaddr, bufp, 0 );
		bp = (short *)bufp->page;
	}
	fprintf ( stderr, "\n" );
}
#endif
