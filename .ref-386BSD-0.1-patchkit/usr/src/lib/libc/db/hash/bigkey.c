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
static char sccsid[] = "@(#)bigkey.c	5.5 (Berkeley) 3/12/91";
#endif /* LIBC_SCCS and not lint */

/******************************************************************************

PACKAGE: hash

DESCRIPTION: 
	Big key/data handling for the hashing package.

ROUTINES: 
    External
	__big_keydata
	__big_split
	__big_insert
	__big_return
	__big_delete
	__find_last_page
    Internal
	collect_key
	collect_data
******************************************************************************/
/* Includes */
#include <sys/param.h>
#include <assert.h>
#include <errno.h>
#include <db.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "hash.h"
#include "page.h"

/* Externals */
/* buf.c */
extern BUFHEAD *__get_buf();

/* dynahash.c */
extern	u_int call_hash();

/* page.c */
extern BUFHEAD *__add_ovflpage();

/* My externals */
extern int __big_keydata();
extern int __big_split();
extern int __big_insert();
extern int __big_return();
extern int __big_delete();
extern u_short __find_last_page();
extern int __find_bigpair();

/* My internals */
static int collect_key();
static int collect_data();

#ifdef HASH_STATISTICS
extern long hash_accesses, hash_collisions, hash_expansions, hash_overflows;
#endif
/*
Big_insert

You need to do an insert and the key/data pair is too big
0 ==> OK
-1 ==> ERROR
*/
extern int
__big_insert ( bufp, key, val )
BUFHEAD *bufp;
DBT	*key, *val;
{
    char	*cp = bufp->page;	/* Character pointer of p */
    register u_short	*p = (u_short *)cp;
    char	*key_data, *val_data;
    int		key_size, val_size;
    int		n;
    u_short	space, move_bytes, off;

    key_data = (char *)key->data;
    key_size = key->size;
    val_data = (char *)val->data;
    val_size = val->size;

    /* First move the Key */
    for ( space = FREESPACE(p) - BIGOVERHEAD; 
	  key_size; 
	  space = FREESPACE(p) - BIGOVERHEAD ) {
	move_bytes = MIN(space, key_size);
	off = OFFSET(p) - move_bytes;
	bcopy (key_data, cp+off, move_bytes );
	key_size -= move_bytes;
	key_data += move_bytes;
	n = p[0];
	p[++n] = off;
	p[0] = ++n;
	FREESPACE(p) = off - PAGE_META(n);
	OFFSET(p) = off;
	p[n] = PARTIAL_KEY;
	bufp = __add_ovflpage(bufp);
	if ( !bufp ) {
	    return(-1);
	}
	n = p[0];
	if ( !key_size ) {
	    if ( FREESPACE(p) ) {
		move_bytes = MIN (FREESPACE(p), val_size);
		off = OFFSET(p) - move_bytes;
		p[n] = off;
		bcopy ( val_data, cp + off, move_bytes );
		val_data += move_bytes;
		val_size -= move_bytes;
		p[n-2] = FULL_KEY_DATA;
		FREESPACE(p) = FREESPACE(p) - move_bytes;
		OFFSET(p) = off;
	    }
	    else p[n-2] = FULL_KEY;
	}
	p = (u_short *)bufp->page;
	cp = bufp->page;
	bufp->flags |= BUF_MOD;
    }

    /* Now move the data */
    for ( space = FREESPACE(p) - BIGOVERHEAD; 
	  val_size; 
	  space = FREESPACE(p) - BIGOVERHEAD ) {
	move_bytes = MIN(space, val_size);
	/*
	    Here's the hack to make sure that if the data ends
	    on the same page as the key ends, FREESPACE is
	    at least one
	*/
	if ( space == val_size && val_size == val->size ) {
	    move_bytes--;
	}
	off = OFFSET(p) - move_bytes;
	bcopy (val_data, cp+off, move_bytes );
	val_size -= move_bytes;
	val_data += move_bytes;
	n = p[0];
	p[++n] = off;
	p[0] = ++n;
	FREESPACE(p) = off - PAGE_META(n);
	OFFSET(p) = off;
	if ( val_size ) {
	    p[n] = FULL_KEY;
	    bufp = __add_ovflpage (bufp);
	    if ( !bufp ) {
		return(-1);
	    }
	    cp = bufp->page;
	    p = (u_short *)cp;
	} else {
	    p[n] = FULL_KEY_DATA;
	}
	bufp->flags |= BUF_MOD;
    }
    return(0);
}

/*
    Called when bufp's page  contains a partial key (index should be 1)

    All pages in the big key/data pair except bufp are freed.  We cannot
    free bufp because the page pointing to it is lost and we can't
    get rid of its pointer.

    Returns 0 => OK
	    -1 => ERROR
*/
extern int
__big_delete (bufp, ndx)
BUFHEAD	*bufp;
int	ndx;
{
	register	BUFHEAD		*rbufp = bufp;
	register	BUFHEAD		*last_bfp = NULL;
	char	*cp;
	u_short	*bp = (u_short *)bufp->page;
	u_short	*xbp;
	u_short	pageno = 0;
	u_short	off, free_sp;
	int	key_done = 0;
	int	n;

	while (!key_done || (bp[2] != FULL_KEY_DATA)) {
	    if ( bp[2] == FULL_KEY || bp[2] == FULL_KEY_DATA ) key_done = 1;

	    /*
		If there is freespace left on a FULL_KEY_DATA page,
		then the data is short and fits entirely on this
		page, and this is the last page.
	    */
	    if ( bp[2] == FULL_KEY_DATA && FREESPACE(bp) ) break;
	    pageno = bp[bp[0]-1];
	    rbufp->flags |= BUF_MOD;
	    rbufp = __get_buf ( pageno, rbufp, 0 );
	    if ( last_bfp ) __free_ovflpage(last_bfp);
	    last_bfp = rbufp;
	    if ( !rbufp ) return(-1);			/* Error */
	    bp = (u_short *)rbufp->page;
	}

	/* 
	    If we get here then rbufp points to the last page of
	    the big key/data pair.  Bufp points to the first
	    one -- it should now be empty pointing to the next
	    page after this pair.  Can't free it because we don't
	    have the page pointing to it.
	*/

	/* This is information from the last page of the pair */
	n = bp[0];
	pageno = bp[n-1];

	/* Now, bp is the first page of the pair */
	bp = (u_short *)bufp->page;
	if ( n > 2 ) {
	    /* There is an overflow page */
	    bp[1] = pageno;
	    bp[2] = OVFLPAGE;
	    bufp->ovfl = rbufp->ovfl;
	} else {
	    /* This is the last page */
	    bufp->ovfl = NULL;
	}
	n -= 2;
	bp[0] = n;
	FREESPACE(bp) = hashp->BSIZE - PAGE_META(n);
	OFFSET(bp) = hashp->BSIZE - 1;

	bufp->flags |= BUF_MOD;
	if ( rbufp ) __free_ovflpage(rbufp);
	if ( last_bfp != rbufp ) __free_ovflpage(last_bfp);

	hashp->NKEYS--;
	return(0);
}

/*
    0 = key not found
    -1 = get next overflow page
    -2 means key not found and this is big key/data
    -3 error
*/
extern int
__find_bigpair(bufp, ndx, key, size )
BUFHEAD	*bufp;
int	ndx;
char	*key;
int	size;
{
    register	u_short	*bp = (u_short *)bufp->page;
    register	char	*p = bufp->page;
    int		ksize = size;
    char	*kkey = key;
    u_short	bytes;


    for ( bytes = hashp->BSIZE - bp[ndx]; 
	  bytes <= size && bp[ndx+1] == PARTIAL_KEY; 
	  bytes = hashp->BSIZE - bp[ndx] ) {

	if ( bcmp ( p+bp[ndx], kkey, bytes ))return(-2);
	kkey += bytes;
	ksize -= bytes;
	bufp = __get_buf ( bp[ndx+2], bufp, 0 );
	if ( !bufp ) {
	    return(-3);
	}
	p = bufp->page;
	bp = (u_short *)p;
	ndx = 1;
    }

    if ( (bytes != ksize) || bcmp ( p+bp[ndx], kkey, bytes )) {
#ifdef HASH_STATISTICS
	hash_collisions++;
#endif
	return(-2);
    }
    else return (ndx);
}


/*
    Given the buffer pointer of the first overflow page of a big pair, 
    find the end of the big pair

    This will set bpp to the buffer header of the last page of the big pair.  
    It will return the pageno of the overflow page following the last page of 
    the pair; 0 if there isn't any (i.e. big pair is the last key in the 
    bucket)
*/
extern u_short
__find_last_page ( bpp )
BUFHEAD	**bpp;
{
	int	n;
	u_short	pageno;
	BUFHEAD	*bufp = *bpp;
	u_short	*bp = (u_short *)bufp->page;

	while ( 1 ) {
	    n = bp[0];

	    /*
		This is the last page if:
			the tag is FULL_KEY_DATA and either
				only 2 entries
				OVFLPAGE marker is explicit
				there is freespace on the page
	    */
	    if ( bp[2] == FULL_KEY_DATA &&
		 ((n == 2) ||  (bp[n] == OVFLPAGE) || (FREESPACE(bp)) ) ) break;

	    pageno = bp[n-1];
	    bufp = __get_buf ( pageno, bufp, 0 );
	    if ( !bufp ) return (0);		/* Need to indicate an error! */
	    bp = (u_short *)bufp->page;
	}

	*bpp = bufp;
	if ( bp[0] > 2 ) return ( bp[3] );
	else return(0);
}


/*
    Return the data for the key/data pair
    that begins on this page at this index
    (index should always be 1)
*/
extern int
__big_return ( bufp, ndx, val, set_current )
BUFHEAD	*bufp;
int	ndx;
DBT	*val;
int	set_current;
{
    BUFHEAD	*save_p;
    u_short	save_addr;
    u_short	*bp = (u_short *)bufp->page;
    u_short	off, len;
    char	*cp, *tp;

    while ( bp[ndx+1] == PARTIAL_KEY ) {
	bufp = __get_buf ( bp[bp[0]-1], bufp, 0 );
	if ( !bufp ) return(-1);
	bp = (u_short *)bufp->page;
	ndx = 1;
    }

    if ( bp[ndx+1] == FULL_KEY ) {
	bufp = __get_buf ( bp[bp[0]-1], bufp, 0 );
	if ( !bufp ) return(-1);
	bp = (u_short *)bufp->page;
	save_p = bufp;
	save_addr = save_p->addr;
	off = bp[1];
	len = 0;
    } else if (!FREESPACE(bp)) {
	/*
	    This is a hack.  We can't distinguish between
	    FULL_KEY_DATA that contains complete data or
	    incomplete data, so we require that if the
	    data  is complete, there is at least 1 byte
	    of free space left.
	*/
	off = bp[bp[0]];
	len = bp[1] - off;
	save_p = bufp;
	save_addr = bufp->addr;
	bufp = __get_buf ( bp[bp[0]-1], bufp, 0 );
	if ( !bufp ) return(-1);
	bp = (u_short *)bufp->page;
    } else {
	/* The data is all on one page */
	tp = (char *)bp;
	off = bp[bp[0]];
	val->data = (u_char *)tp + off;
	val->size = bp[1] - off;
	if ( set_current ) {
	    if ( bp[0] == 2 ) {		/* No more buckets in chain */
		hashp->cpage = NULL;
		hashp->cbucket++;
		hashp->cndx=1;
	    } else  {
		hashp->cpage = __get_buf ( bp[bp[0]-1], bufp, 0 );
		if ( !hashp->cpage )return(-1);
		hashp->cndx = 1;
		if ( !((u_short *)hashp->cpage->page)[0] ) {
		    hashp->cbucket++;
		    hashp->cpage = NULL;
		}
	    }
	}
	return(0);
    }
    
    val->size = collect_data ( bufp, len, set_current );
    if ( val->size == -1 ) {
	return(-1);
    }
    if ( save_p->addr != save_addr ) {
	/* We are pretty short on buffers */
	errno = EINVAL;		/* OUT OF BUFFERS */
	return(-1);
    }
    bcopy ( (save_p->page)+off, hashp->tmp_buf, len );
    val->data = (u_char *)hashp->tmp_buf;
    return(0);
}

/*
    Count how big the total datasize is by
    recursing through the pages.  Then allocate
    a buffer and copy the data as you recurse up.
*/
static int
collect_data ( bufp, len, set )
BUFHEAD	*bufp;
int	len;
int	set;
{
    register	char	*p = bufp->page;
    register	u_short	*bp = (u_short *)p;
    u_short	save_addr;
    int	mylen, totlen;
    BUFHEAD	*xbp;

    mylen = hashp->BSIZE - bp[1];
    save_addr = bufp->addr;

    if ( bp[2] == FULL_KEY_DATA ) {	/* End of Data */
	totlen = len + mylen;
	if ( hashp->tmp_buf ) free (hashp->tmp_buf);
	hashp->tmp_buf = (char *)malloc ( totlen );
	if ( !hashp->tmp_buf ) {
	    return(-1);
	}
	if ( set ) {
	    hashp->cndx = 1;
	    if ( bp[0] == 2 ) {		/* No more buckets in chain */
		hashp->cpage = NULL;
		hashp->cbucket++;
	    } else  {
		hashp->cpage = __get_buf ( bp[bp[0]-1], bufp, 0 );
		if (!hashp->cpage) {
		    return(-1);
		} else if ( !((u_short *)hashp->cpage->page)[0] ) {
		    hashp->cbucket++;
		    hashp->cpage = NULL;
		}
	    }
	}
    } else {
	xbp = __get_buf ( bp[bp[0]-1], bufp, 0 );
	if ( !xbp || ((totlen = collect_data ( xbp, len + mylen, set )) < 1) ) {
	    return(-1);
	}
    }
    if ( bufp->addr != save_addr ) {
	errno = EINVAL;		/* Out of buffers */
	return(-1);
    }
    bcopy ( (bufp->page) + bp[1], &hashp->tmp_buf[len], mylen );
    return ( totlen );
}

/*
	Fill in the key and data
	for this big pair 
*/
extern int
__big_keydata ( bufp, ndx, key, val, set )
BUFHEAD	*bufp;
int	ndx;
DBT	*key, *val;
int	set;
{
    key->size = collect_key ( bufp, 0, val, set );
    if ( key->size == -1 ) {
	return (-1);
    }
    key->data = (u_char *)hashp->tmp_key;
    return(0);
}

/*
    Count how big the total key size is by
    recursing through the pages.  Then collect
    the data, allocate a buffer and copy the key as
    you recurse up.
*/
static int
collect_key ( bufp, len, val, set )
BUFHEAD	*bufp;
int	len;
DBT	*val;
int	set;
{
    char	*p = bufp->page;
    u_short	*bp = (u_short *)p;
    u_short	save_addr;
    int	mylen, totlen;
    BUFHEAD	*xbp;

    mylen = hashp->BSIZE - bp[1];

    save_addr = bufp->addr;
    totlen = len + mylen;
    if ( bp[2] == FULL_KEY || bp[2] == FULL_KEY_DATA ) {/* End of Key */
	if ( hashp->tmp_key ) free (hashp->tmp_key);
	hashp->tmp_key = (char *)malloc ( totlen );
	if ( !hashp->tmp_key ) {
	    return(-1);
	}
	__big_return ( bufp, 1, val, set );
    } else {
	xbp = __get_buf (bp[bp[0]-1], bufp, 0);
	if ( !xbp || ((totlen = collect_key (xbp, totlen, val, set)) < 1 ) ) {
	    return(-1);
	}
    }
    if ( bufp->addr != save_addr ) {
	errno = EINVAL;		/* MIS -- OUT OF BUFFERS */
	return (-1);
    }
    bcopy ( (bufp->page) + bp[1], &hashp->tmp_key[len], mylen );
    return ( totlen );
}


/*
    return 0 => OK
	   -1 => error
*/
extern int
__big_split ( op, np, big_keyp, addr, obucket, ret )
BUFHEAD	*op;		/* Pointer to where to put keys that go in old bucket */
BUFHEAD	*np;		/* Pointer to new bucket page */
BUFHEAD	*big_keyp;	/* Pointer to first page containing the big key/data */
u_short	addr;		/* Address of big_keyp */
u_int	obucket;	/* Old Bucket */
SPLIT_RETURN	*ret;
{
    register	u_short	*prev_pagep;
    register	BUFHEAD	*tmpp;
    register	u_short 	*tp;
    BUFHEAD	*bp = big_keyp;
    u_short	off, free_space;
    u_short	n;

    DBT		key, val;

    u_int	change;

    /* Now figure out where the big key/data goes */
    if (__big_keydata ( big_keyp, 1, &key, &val, 0 )) {
	return(-1);
    }
    change = (__call_hash ( key.data, key.size ) != obucket );

    if ( ret->next_addr = __find_last_page ( &big_keyp ) ) {
	if (!(ret->nextp = __get_buf ( ret->next_addr, big_keyp, 0 ))) {
	    return(-1);;
	}
    } else {
	ret->nextp = NULL;
    }

    /* Now make one of np/op point to the big key/data pair */
    assert(np->ovfl == NULL);
    if ( change ) tmpp = np;
    else tmpp = op;

    tmpp->flags |= BUF_MOD;
#ifdef DEBUG1
    fprintf ( stderr, "BIG_SPLIT: %d->ovfl was %d is now %d\n", tmpp->addr,
		(tmpp->ovfl?tmpp->ovfl->addr:0), 
		(bp?bp->addr:0) );
#endif
    tmpp->ovfl = bp;		/* one of op/np point to big_keyp */
    tp = (u_short *)tmpp->page;
    assert ( FREESPACE(tp) >= OVFLSIZE);
    n = tp[0];
    off = OFFSET(tp);
    free_space = FREESPACE(tp);
    tp[++n] = addr;
    tp[++n] = OVFLPAGE;
    tp[0] = n;
    OFFSET(tp) = off;
    FREESPACE(tp) = free_space - OVFLSIZE;

    /* 
	Finally, set the new and old return values.
	BIG_KEYP contains a pointer to the last page of the big key_data pair.
	Make sure that big_keyp has no following page (2 elements) or create
	an empty following page.
    */

    ret->newp = np;
    ret->oldp = op;

    tp = (u_short *)big_keyp->page;
    big_keyp->flags |= BUF_MOD;
    if ( tp[0] > 2 ) {
	/* 
	    There may be either one or two offsets on this page 
	    If there is one, then the overflow page is linked on
	    normally and tp[4] is OVFLPAGE.  If there are two, tp[4]
	    contains the second offset and needs to get stuffed in
	    after the next overflow page is added
	*/
	n = tp[4];		
	free_space = FREESPACE(tp);
	off = OFFSET(tp);
	tp[0] -= 2;
	FREESPACE(tp) = free_space + OVFLSIZE;
	OFFSET(tp) = off;
	tmpp = __add_ovflpage ( big_keyp );
	if ( !tmpp ) {
	    return(-1);
	}
	tp[4] = n;
    } else {
	tmpp = big_keyp;
    }

    if ( change ) ret->newp = tmpp;
    else ret->oldp = tmpp;

    return(0);
}
