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
static char sccsid[] = "@(#)hash.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <errno.h>
#include <assert.h>
#include <db.h>
#include "hash.h"
#include <string.h>

/* For systems that do not have O_ACCMODE */
#ifndef O_ACCMODE
#define O_ACCMODE       (O_RDONLY|O_WRONLY|O_RDWR)
#endif

/* Fast arithmetic, relying on powers of 2, */

#define MOD(x,y)		((x) & ((y)-1))
#define RETURN_ERROR(ERR,LOC)	{ save_errno = ERR; goto LOC; }

/* Return values */

#define	SUCCESS	0
#define	ERROR	-1
#define	ABNORMAL 1

/* external routines */
extern char *calloc();

/* page.c */
extern int __get_page();
extern int __split_page();
extern int __addel();
extern int __delpair();
extern u_long *__init_bitmap();

/* big.c */
extern void __big_return();
extern int __big_keydata();
extern u_short __find_last_page();

/* buf.c */
extern BUFHEAD	*__get_buf();
extern void __buf_init();
extern int __buf_free();

/* hash function */
extern int (*default_hash)();

/* My externals */
extern int __expand_table();
extern int __call_hash();

/* Internal routines */
static HTAB *init_hash();
static int hash_access();
static int flush_meta();
static int alloc_segs();
static int init_htab();
static char *hash_realloc();
static int hdestroy();
static int hash_put();
static int hash_delete();
static int hash_get();
static int hash_sync();
static int hash_close();
static int hash_seq();
static void swap_header_copy();
static void swap_header();

/* Local data */

HTAB *hashp = NULL;

#ifdef HASH_STATISTICS
long hash_accesses, hash_collisions, hash_expansions, hash_overflows;
#endif

/************************** INTERFACE ROUTINES **********************/
/* OPEN/CLOSE */

extern	DB *
hash_open ( file, flags, mode, info )
char	*file;
int	flags;
int	mode;
HASHINFO	*info;		/* Special directives for create */
{
    int		buckets;
    int		bpages;
    int		hdrsize;
    int		i;
    int		new_table = 0;
    int		nkey;
    int		nsegs;
    int		save_errno;
    struct	stat statbuf;
    DB		*dbp;


    if ( !(hashp = (HTAB *) calloc( 1, sizeof(HTAB) ))) {
	/* calloc should set errno */
	return(0);
    }
    /* 
	Select flags relevant to us.
	Even if user wants write only, we need to be able to read 
	the actual file, so we need to open it read/write.  But, the
	field in the hashp structure needs to be accurate so that
	we can check accesses.
    */
    flags = flags & (O_CREAT | O_EXCL | O_RDONLY | O_RDWR | O_TRUNC | O_WRONLY);
    hashp->flags = flags;
    if ( flags & O_WRONLY )  flags = (flags & ~O_WRONLY) | O_RDWR;

    if ( !file || 
	 (flags & O_TRUNC) || 
	 (stat ( file, &statbuf ) && (errno == ENOENT)) ) {
	 new_table = 1;
    }

    if ( file && ((hashp->fp = open ( file, flags, mode )) == -1)) {
	RETURN_ERROR (errno, error0);
    }

    if ( new_table ) {
	if ( !(hashp = init_hash( info )) ) {
	    RETURN_ERROR(errno,error1);
	}
    } else {
	/* Table already exists */
	if ( info && info->hash ) hashp->hash = info->hash;
	else hashp->hash = default_hash;

	hdrsize = read ( hashp->fp, &hashp->hdr, sizeof(HASHHDR) );
#if BYTE_ORDER == LITTLE_ENDIAN
	swap_header ( );
#endif
	if ( hdrsize == -1 ) {
	    RETURN_ERROR(errno, error1);
	}
	if ( hdrsize != sizeof(HASHHDR) ) {
	    RETURN_ERROR(EFTYPE, error1);
	}

	/* Verify file type, versions and hash function */
	if ( hashp->MAGIC != HASHMAGIC ) 
	    RETURN_ERROR ( EFTYPE, error1 );
	if ( hashp->VERSION != VERSION_NO ) 
	    RETURN_ERROR ( EFTYPE, error1 );
	if (hashp->hash ( CHARKEY, sizeof(CHARKEY) ) != hashp->H_CHARKEY ) {
	    RETURN_ERROR ( EFTYPE, error1 );
	}

	/* 
	    Figure out how many segments we need.
	*/
	nsegs = (hashp->MAX_BUCKET + hashp->SGSIZE -1)/ hashp->SGSIZE;
	hashp->nsegs = 0;
	if (alloc_segs( nsegs )) {	
	    /* 
		If alloc_segs fails, table will have been destroyed 
		and errno will have been set.
	    */
	    return( (DB *) NULL );
	}

	/* Read in bitmaps */
	bpages = (hashp->SPARES[__log2(hashp->MAX_BUCKET)] + 
		  (hashp->BSIZE << BYTE_SHIFT) - 1) >> 
		  (hashp->BSHIFT + BYTE_SHIFT);

	hashp->mapp[0] = (u_long *)malloc(bpages<<hashp->BSHIFT);
	if ( !hashp->mapp[0] ) {
	    RETURN_ERROR(errno, error2);
	}
	for ( i = 0; i < bpages; i++ ) {
	    hashp->mapp[i] = &hashp->mapp[0][i<<hashp->BSHIFT];
	    if (__get_page ((char *)hashp->mapp[i],
		hashp->BITMAPS[i], 0, 1, 1)) {
		    RETURN_ERROR(errno, error2);
	    }
	}

    }

    /* Initialize Buffer Manager */
    if ( info && info->ncached ) {
	__buf_init (info->ncached);
    } else {
	__buf_init (DEF_BUFSIZE);
    }

    hashp->new_file = new_table;
    hashp->save_file = (file != NULL);
    hashp->cbucket = -1;
    if ( !(dbp = (DB *)malloc ( sizeof (DB) )) ) {
	save_errno = errno;
	hdestroy();
	errno = save_errno;
	return ( NULL );
    }
    dbp->internal = (char *)hashp;
    dbp->close = hash_close;
    dbp->delete = hash_delete;
    dbp->get = hash_get;
    dbp->put = hash_put;
    dbp->seq = hash_seq;
    dbp->sync = hash_sync;

#ifdef DEBUG
	(void)fprintf(stderr, "%s\n%s%x\n%s%d\n%s%d\n%s%d\n%s%d\n%s%d\n%s%d\n%s%d\n%s%x\n%s%x\n%s%d\n%s%d\n",
		"init_htab:",
		"TABLE POINTER   ", hashp,
		"BUCKET SIZE     ", hashp->BSIZE,
		"BUCKET SHIFT    ", hashp->BSHIFT,
		"DIRECTORY SIZE  ", hashp->DSIZE,
		"SEGMENT SIZE    ", hashp->SGSIZE,
		"SEGMENT SHIFT   ", hashp->SSHIFT,
		"FILL FACTOR     ", hashp->FFACTOR,
		"MAX BUCKET      ", hashp->MAX_BUCKET,
		"HIGH MASK       ", hashp->HIGH_MASK,
		"LOW  MASK       ", hashp->LOW_MASK,
		"NSEGS           ", hashp->nsegs,
		"NKEYS           ", hashp->NKEYS );
#endif
#ifdef HASH_STATISTICS
	hash_overflows = hash_accesses = hash_collisions = hash_expansions = 0;
#endif
    return (dbp);

error2:
    (void)hdestroy();
    errno = save_errno;
    hashp->errno = errno;
    return ( (DB *)NULL );

error1:
    (void) close ( hashp->fp );

error0:
    free ( hashp );
    errno = save_errno;
    hashp->errno = errno;
    return ( (DB *) NULL );
}

static int
hash_close (dbp)
DB	*dbp;
{
	if ( !dbp ) {
	    return(ERROR);
	}
	hashp = (HTAB *)dbp->internal;
	return (hdestroy());
}


/************************** LOCAL CREATION ROUTINES **********************/
static HTAB * 
init_hash( info )
HASHINFO *info;
{
	int	nelem;

	nelem = 1;

	hashp->LORDER	= BYTE_ORDER;
	hashp->BSIZE    = DEF_BUCKET_SIZE;
	hashp->BSHIFT   = DEF_BUCKET_SHIFT;
	hashp->SGSIZE   = DEF_SEGSIZE;
	hashp->SSHIFT   = DEF_SEGSIZE_SHIFT;
	hashp->DSIZE    = DEF_DIRSIZE;
	hashp->FFACTOR  = DEF_FFACTOR;
	hashp->hash     = default_hash;
	bzero (hashp->SPARES, sizeof (hashp->SPARES));

	if ( info ) {
	    if ( info->bsize )   {
		/* Round pagesize up to power of 2 */
		hashp->BSHIFT  = __log2(info->bsize);
		hashp->BSIZE   = 1 << hashp->BSHIFT;
		if ( hashp->BSIZE > MAX_BSIZE ) {
		    errno = EINVAL;
		    return(0);
		}
	    }
	    if ( info->ffactor )  hashp->FFACTOR = info->ffactor;
	    if ( info->hash ) hashp->hash    = info->hash;
	    if ( info->nelem ) nelem = info->nelem;
	    if ( info->lorder ) {
		if ( info->lorder != BIG_ENDIAN && 
		     info->lorder != LITTLE_ENDIAN) {
		    errno = EINVAL;
		    return(0);
		}
		hashp->LORDER = info->lorder;
	    }
	}

	/* init_htab should destroy the table and set errno if it fails */
	if ( init_htab ( nelem ) ) {
	    return(0);
	} else {
	    return(hashp);
	}
}

/*
    This calls alloc_segs which may run out of memory.
    Alloc_segs will destroy the table and set errno,
    so we just pass the error information along.

    Returns 0 on No Error

*/
static int
init_htab ( nelem )
int	nelem;
{
	register SEGMENT	*segp;
	register int nbuckets;
	register int nsegs;
	int	l2;

 /*
  * Divide number of elements by the fill factor and determine a desired
  * number of buckets.  Allocate space for the next greater power of
  * two number of buckets
  */
	nelem = (nelem - 1) / hashp->FFACTOR + 1;

	l2 = __log2(nelem);
	nbuckets = 1 << l2;
	nbuckets = MAX ( nbuckets, 2 );

	hashp->SPARES[l2] = l2 + 1;
	hashp->SPARES[l2+1] = l2 + 1;
	/* 
	    First bitmap page is at:
		splitpoint l2
		page offset 1
	*/
	__init_bitmap ( OADDR_OF(l2,1), l2+1, 0 );

	hashp->MAX_BUCKET = hashp->LOW_MASK = nbuckets - 1;
	hashp->HIGH_MASK = (nbuckets << 1) - 1;
	hashp->HDRPAGES = ((MAX(sizeof(HASHHDR),MINHDRSIZE) - 1) >> 
			  hashp->BSHIFT) + 1;

	nsegs = (nbuckets - 1) / hashp->SGSIZE + 1;
	nsegs = 1 << __log2(nsegs);

	if ( nsegs > hashp->DSIZE ) {
	    hashp->DSIZE  = nsegs;
	}

	return (alloc_segs ( nsegs ) );
}

/********************** DESTROY/CLOSE ROUTINES ************************/

/*
    Flushes any changes to the file if necessary and
    destroys the hashp structure, freeing all allocated
    space.
*/
static int
hdestroy()
{
	int	save_errno;
	int	i;

	save_errno = 0;

	if (hashp != NULL) {
#ifdef HASH_STATISTICS
	 (void)	fprintf(stderr,	"hdestroy: accesses %ld collisions %ld\n",
			hash_accesses, hash_collisions);
	 (void)	fprintf(stderr, "hdestroy: expansions %ld\n",
			hash_expansions);
	 (void)	fprintf(stderr, "hdestroy: overflows %ld\n",
			hash_overflows);
	 (void)	fprintf(stderr,	"keys %ld maxp %d segmentcount %d\n",
			hashp->NKEYS, hashp->MAX_BUCKET, hashp->nsegs);

	for ( i = 0; i < NCACHED; i++ ) {
	    (void) fprintf ( stderr, "spares[%d] = %d\n", i, hashp->SPARES[i] );
	}
#endif

		/* 
		    Call on buffer manager to free buffers, and if
		    required, write them to disk
		*/
		if (__buf_free(1, hashp->save_file)) {
		    save_errno = errno;
		}
		if ( hashp->dir ) {
		    (void)free(*hashp->dir);	/* Free initial segments */
		    /* Free extra segments */
		    while ( hashp->exsegs-- ) {
			(void)free ( hashp->dir[--hashp->nsegs] );
		    }
		    (void)free(hashp->dir);
		}
		if (flush_meta() && !save_errno) {
		    save_errno = errno;
		}
		if ( hashp->save_file ) (void)close (hashp->fp);
		(void)free(hashp);
		hashp = NULL;
	}
	if ( save_errno ) {
	    errno = save_errno;
	    return(ERROR);
	} else {
	    return(SUCCESS);
	}
}

/* 
    Write modified pages to disk 
    0 == OK
    -1 ERROR
*/
static int
hash_sync(dbp)
DB	*dbp;
{
	if ( !dbp ) {
	    return (ERROR);
	}

	hashp = (HTAB *)dbp->internal;

	if (!hashp->save_file) return(0);
	if ( __buf_free ( 0, 1 )  || flush_meta()) {
	    return(ERROR);
	}
	hashp->new_file = 0;
	return(0);
}

/*
0 == OK
-1 indicates that errno should be set
*/
static int
flush_meta()
{
    	int	fp;
	int	hdrsize;
	int	i;
	int	wsize;
	HASHHDR	*whdrp;
	HASHHDR	whdr;

	if (!hashp->save_file) return(0);
	hashp->MAGIC = HASHMAGIC;
	hashp->VERSION = VERSION_NO;
	hashp->H_CHARKEY = hashp->hash ( CHARKEY, sizeof(CHARKEY));

	fp = hashp->fp;
	whdrp = &hashp->hdr;
#if BYTE_ORDER == LITTLE_ENDIAN
	whdrp = &whdr;
	swap_header_copy( &hashp->hdr, whdrp );
#endif
	if ( (lseek (fp, 0, L_SET) == -1 ) ||
	     ((wsize = write ( fp, whdrp, sizeof(HASHHDR))) == -1)) {
	    return(-1);
	} else if ( wsize != sizeof(HASHHDR) ) {
	    errno = EFTYPE;
	    hashp->errno = errno;
	    return(-1);
	}
	for ( i = 0; i < NCACHED; i++ ) {
	    if ( hashp->mapp[i] ) {
		if (!__put_page((char *)hashp->mapp[i],
		    hashp->BITMAPS[i], 0, 1)){
		    return(-1);
		}
	    }
	}
	return(0);
}
/*******************************SEARCH ROUTINES *****************************/
/*
    All the access routines return 
	0 on SUCCESS
	1 to indicate an external ERROR (i.e. key not found, etc)
	-1 to indicate an internal ERROR (i.e. out of memory, etc)
*/
static int
hash_get ( dbp, key, data )
DB	*dbp;
DBT *key, *data;
{
    if ( !dbp ) {
	return (ERROR);
    }
    hashp = (HTAB *)dbp->internal;
    if ( hashp->flags & O_WRONLY ) {
	errno = EBADF;
	hashp->errno = errno;
	return ( ERROR );
    }
    return ( hash_access ( HASH_GET, key, data ) );
}

static int
hash_put ( dbp, key, data, flag )
DB	*dbp;
DBT 	*key, *data;
int	flag;
{
    if ( !dbp ) {
	return (ERROR);
    }
    hashp = (HTAB *)dbp->internal;
    if ( (hashp->flags & O_ACCMODE) == O_RDONLY ) {
	errno = EBADF;
	hashp->errno = errno;
	return ( ERROR );
    }
    if ( flag == R_NOOVERWRITE ) {
	return ( hash_access ( HASH_PUTNEW, key, data ) );
    } else {
	return ( hash_access ( HASH_PUT, key, data ) );
    }
}

static int
hash_delete ( dbp, key, flags )
DB	*dbp;
DBT 	*key;
int	flags;		/* Ignored */
{
    if ( !dbp ) {
	return (ERROR);
    }
    hashp = (HTAB *)dbp->internal;
    if ( (hashp->flags & O_ACCMODE) == O_RDONLY ) {
	errno = EBADF;
	hashp->errno = errno;
	return ( ERROR );
    }
    return ( hash_access ( HASH_DELETE, key, NULL ) );
}

/*
    Assume that hashp has been set in wrapper routine
*/
static int
hash_access(action, key, val)
ACTION action;
DBT *key, *val;
{
	register	BUFHEAD	*rbufp;
	register	u_short	*bp;
	register	int	ndx;
	register 	int n;
	register 	int off = hashp->BSIZE;
	register	int		size;
	register	char	*kp;
	BUFHEAD	*bufp;
	u_short	pageno;

#ifdef HASH_STATISTICS
	hash_accesses++;
#endif

	size = key->size;
	kp = key->data;
	rbufp = __get_buf ( __call_hash(key->data, size), NULL, 0 );
	if ( !rbufp ) return(ERROR);

	for ( bp = (u_short *)rbufp->page, n = *bp++, ndx = 1; ndx < n;  ) {
	    if ( bp[1] >= REAL_KEY ) {
		/* Real key/data pair */
		if (size == off - *bp && 
		    bcmp(kp, rbufp->page + *bp, size) == 0) {
		    goto found;
		}
		off = bp[1];
#ifdef HASH_STATISTICS
		hash_collisions++;
#endif
	        bp += 2; 
	        ndx += 2;
	    } else if ( bp[1] == OVFLPAGE ) {
		rbufp = __get_buf ( *bp, rbufp, 0 );
		if ( !rbufp ) return (ERROR);
		/* FOR LOOP INIT */
		bp = (u_short *)rbufp->page;
		n = *bp++;
		ndx = 1;
		off = hashp->BSIZE;
	    } else if ( bp[1] < REAL_KEY ) {
		if ( (ndx = __find_bigpair(rbufp, ndx, kp, size )) > 0 ) {
		    goto found;
		} else if ( ndx == -2 ) {
		    bufp = rbufp;
		    if ( !(pageno = __find_last_page ( &bufp )) ) {
			ndx = 0;
			rbufp = bufp;
			break;	/* FOR */
		    }
		    rbufp = __get_buf ( pageno, bufp, 0 );
		    if ( !rbufp ) return (ERROR);
		    /* FOR LOOP INIT */
		    bp = (u_short *)rbufp->page;
		    n = *bp++;
		    ndx = 1;
		    off = hashp->BSIZE;
		} else  {
		    return(ERROR);
		}
	    } 
	}

	/* Not found */
	switch ( action ) {
	    case HASH_PUT:
	    case HASH_PUTNEW:
		if (__addel(rbufp, key, val)) {
		    return(ERROR);
		} else {
		    return(SUCCESS);
		}
	    case HASH_GET:
		return ( ABNORMAL );
	    case HASH_DELETE:
		return ( ABNORMAL );
	    default:
		return(ABNORMAL);
	}

found:
	switch (action) {
	    case HASH_PUTNEW:
		return(ABNORMAL);
	    case HASH_GET:
		bp = (u_short *)rbufp->page;
		if (bp[ndx+1] < REAL_KEY) __big_return(rbufp, ndx, val, 0);
		else {
		    val->data = rbufp->page + (int) bp[ndx + 1];
		    val->size = bp[ndx] - bp[ndx + 1];
		}
		break;
	    case HASH_PUT:
		if (__delpair (rbufp, ndx))return(ERROR);
		if (__addel (rbufp, key, val)) return(ERROR);
		break;
	    case HASH_DELETE:
		if (__delpair (rbufp, ndx))return(ERROR);
		break;
	}
	return (SUCCESS);
}

static int
hash_seq(dbp, key, data, flag)
DB	*dbp;
DBT 	*key, *data;
int flag;
{
	register	int bucket;
	register	BUFHEAD	*bufp;
	u_short	*bp;
	u_short	ndx;
	u_short	pageno;

	if ( !dbp ) {
	    return (ERROR);
	}

	hashp = (HTAB *)dbp->internal;
	if ( hashp->flags & O_WRONLY ) {
	    errno = EBADF;
	    hashp->errno = errno;
	    return ( ERROR );
	}
#ifdef HASH_STATISTICS
	hash_accesses++;
#endif

	if ( (hashp->cbucket < 0) || (flag == R_FIRST) ) {
	    hashp->cbucket = 0;
	    hashp->cndx = 1;
	    hashp->cpage = NULL;
	}

	if ( !(bufp = hashp->cpage) ) {
	    for (bucket = hashp->cbucket; 
		 bucket <= hashp->MAX_BUCKET; 
		 bucket++, hashp->cndx = 1 ) {

		bufp = __get_buf ( bucket, NULL, 0 );
		if (!bufp) return(ERROR);
		hashp->cpage = bufp;
		bp = (u_short *)bufp->page;
		if (bp[0]) break;
	    }
	    hashp->cbucket = bucket;
	    if ( hashp->cbucket > hashp->MAX_BUCKET ) {
		hashp->cbucket = -1;
		return ( ABNORMAL );
	    } 
	} else {
	    bp  = (u_short *)hashp->cpage->page;
	}
	
#ifdef DEBUG
	assert (bp);
	assert (bufp);
#endif
	while ( bp[hashp->cndx+1] == OVFLPAGE ) {
	    bufp = hashp->cpage = __get_buf ( bp[hashp->cndx], bufp, 0 );
	    if (!bufp) return(ERROR);
	    bp = (u_short *)(bufp->page);
	    hashp->cndx = 1;
	}
	ndx = hashp->cndx;
	if (bp[ndx+1] < REAL_KEY) {
	    if (__big_keydata(bufp, ndx, key, data, 1)) {
		return(ERROR);
	    }
	} else {
	    key->data = hashp->cpage->page + bp[ndx];
	    key->size = (ndx > 1 ? bp[ndx-1] : hashp->BSIZE) - bp[ndx];
	    data->data = hashp->cpage->page + bp[ndx + 1];
	    data->size = bp[ndx] - bp[ndx + 1];
	    ndx += 2;
	    if ( ndx > bp[0] ) {
		hashp->cpage = NULL;
		hashp->cbucket++;
		hashp->cndx=1;
	    } else hashp->cndx = ndx;
	}
	return (SUCCESS);
}

/********************************* UTILITIES ************************/
/*
    0 ==> OK
    -1 ==> Error
*/
extern int
__expand_table()
{
	int	old_bucket, new_bucket;
	int	new_segnum;
	int	dirsize;
	int	spare_ndx;
	register	char **old, **new;
#ifdef HASH_STATISTICS
	hash_expansions++;
#endif

	new_bucket = ++hashp->MAX_BUCKET;
	old_bucket = (hashp->MAX_BUCKET & hashp->LOW_MASK);

	new_segnum = new_bucket >> hashp->SSHIFT;

	/* Check if we need a new segment */
	if ( new_segnum >= hashp->nsegs ) {

	    /* Check if we need to expand directory */
	    if ( new_segnum >= hashp->DSIZE ) {

		/* Reallocate directory */
		dirsize = hashp->DSIZE * sizeof ( SEGMENT * );
		if (!hash_realloc ( &hashp->dir, dirsize, (dirsize << 1 ) )) {
		    return(-1);
		}
		hashp->DSIZE = dirsize << 1;
	    }
	    if (!(hashp->dir[new_segnum] = 
		    (SEGMENT)calloc ( hashp->SGSIZE, sizeof(SEGMENT)))) {
		    return(-1);
	    }
	    hashp->exsegs++;
	    hashp->nsegs++;
	}

	/*
	    If the split point is increasing (MAX_BUCKET's log
	    base 2 increases), we need to copy the current contents
	    of the spare split bucket to the next bucket
	*/
	spare_ndx = __log2(hashp->MAX_BUCKET);
	if ( spare_ndx != (__log2(hashp->MAX_BUCKET - 1))) {
	    hashp->SPARES[spare_ndx] = hashp->SPARES[spare_ndx-1];
	}

	if ( new_bucket > hashp->HIGH_MASK ) {
	    /* Starting a new doubling */
	    hashp->LOW_MASK = hashp->HIGH_MASK;
	    hashp->HIGH_MASK = new_bucket | hashp->LOW_MASK;
	}

	/*
	 * Relocate records to the new bucket
	 */
	return(__split_page ( old_bucket, new_bucket ));
}
/*
    If realloc guarantees that the pointer is not destroyed
    if the realloc fails, then this routine can go away
*/
static char * 
hash_realloc ( p_ptr, oldsize, newsize )
char	**p_ptr;
int	oldsize;
int	newsize;
{
	register char	*p;

	if (p = (char *)malloc ( newsize ) ) {
		bcopy ( *p_ptr, p, oldsize );
		bzero ( *p_ptr + oldsize, newsize-oldsize );
		free(*p_ptr);
		*p_ptr = p;
	}
	return (p);
}

extern int
__call_hash ( k, len )
char	*k;
int	len;
{
	int	n, bucket;
	n = hashp->hash(k, len);

	bucket = n & hashp->HIGH_MASK;
	if ( bucket > hashp->MAX_BUCKET ) {
	    bucket = bucket & hashp->LOW_MASK;
	}

	return(bucket);
}

/*
    Allocate segment table.  On error, destroy the table
    and set errno.

    Returns 0 on success
*/
static int
alloc_segs ( nsegs )
int	nsegs;
{
    register int	i;
    register SEGMENT	store;

    int	save_errno;

    if (!(hashp->dir = (SEGMENT *)calloc(hashp->DSIZE, sizeof(SEGMENT *)))) {
	save_errno = errno;
	(void)hdestroy();
	errno = save_errno;
	return(-1);
    }

    /* Allocate segments */
    store = (SEGMENT)calloc ( nsegs << hashp->SSHIFT, sizeof (SEGMENT) );
    if ( !store ) {
	save_errno = errno;
	(void)hdestroy();
	errno = save_errno;
	return(-1);
    }

    for ( i=0; i < nsegs; i++, hashp->nsegs++ ) {
	hashp->dir[i] = &store[i<<hashp->SSHIFT];
    }
    return(0);
}

/*
    Hashp->hdr needs to be byteswapped.
*/
static void
swap_header_copy ( srcp, destp )
HASHHDR	*srcp;
HASHHDR	*destp;
{
    int i;

    BLSWAP_COPY(srcp->magic, destp->magic);
    BLSWAP_COPY(srcp->version, destp->version);
    BLSWAP_COPY(srcp->lorder, destp->lorder);
    BLSWAP_COPY(srcp->bsize, destp->bsize);
    BLSWAP_COPY(srcp->bshift, destp->bshift);
    BLSWAP_COPY(srcp->dsize, destp->dsize);
    BLSWAP_COPY(srcp->ssize, destp->ssize);
    BLSWAP_COPY(srcp->sshift, destp->sshift);
    BLSWAP_COPY(srcp->max_bucket, destp->max_bucket);
    BLSWAP_COPY(srcp->high_mask, destp->high_mask);
    BLSWAP_COPY(srcp->low_mask, destp->low_mask);
    BLSWAP_COPY(srcp->ffactor, destp->ffactor);
    BLSWAP_COPY(srcp->nkeys, destp->nkeys);
    BLSWAP_COPY(srcp->hdrpages, destp->hdrpages);
    BLSWAP_COPY(srcp->h_charkey, destp->h_charkey);
    for ( i=0; i < NCACHED; i++ ) {
	BLSWAP_COPY ( srcp->spares[i] , destp->spares[i]);
	BSSWAP_COPY ( srcp->bitmaps[i] , destp->bitmaps[i]);
    }
    return;
}

static void
swap_header ( )
{
    HASHHDR	*hdrp;
    int	i;

    hdrp = &hashp->hdr;

    BLSWAP(hdrp->magic);
    BLSWAP(hdrp->version);
    BLSWAP(hdrp->lorder);
    BLSWAP(hdrp->bsize);
    BLSWAP(hdrp->bshift);
    BLSWAP(hdrp->dsize);
    BLSWAP(hdrp->ssize);
    BLSWAP(hdrp->sshift);
    BLSWAP(hdrp->max_bucket);
    BLSWAP(hdrp->high_mask);
    BLSWAP(hdrp->low_mask);
    BLSWAP(hdrp->ffactor);
    BLSWAP(hdrp->nkeys);
    BLSWAP(hdrp->hdrpages);
    BLSWAP(hdrp->h_charkey);
    for ( i=0; i < NCACHED; i++ ) {
	BLSWAP ( hdrp->spares[i] );
	BSSWAP ( hdrp->bitmaps[i] );
    }
    return;
}
