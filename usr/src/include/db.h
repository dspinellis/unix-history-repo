/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)db.h	5.19 (Berkeley) %G%
 */

#ifndef _DB_H_
#define	_DB_H_

#include <sys/types.h>
#include <sys/cdefs.h>

#define	RET_ERROR	-1		/* Return values. */
#define	RET_SUCCESS	 0
#define	RET_SPECIAL	 1

#define	MAX_PAGE_NUMBER	ULONG_MAX	/* >= # of pages in a file */
typedef u_long	pgno_t;
#define	MAX_PAGE_OFFSET	USHRT_MAX	/* >= # of bytes in a page */
typedef u_short	index_t;
#define	MAX_REC_NUMBER	ULONG_MAX	/* >= # of records in a tree */
typedef u_long	recno_t;

/* Key/data structure -- a Data-Base Thang. */
typedef struct {
	void	*data;			/* data */
	size_t	 size;			/* data length */
} DBT;

/* Routine flags. */
#define	R_CURSOR	1		/* del, put, seq */
#define	R_CURSORLOG	2		/* put (RECNO) */
#define	R_FIRST		3		/* seq */
#define	R_IAFTER	4		/* put (RECNO) */
#define	R_IBEFORE	5		/* put (RECNO) */
#define	R_LAST		6		/* seq (BTREE, RECNO) */
#define	R_NEXT		7		/* seq */
#define	R_NOOVERWRITE	8		/* put */
#define	R_PREV		9		/* seq (BTREE, RECNO) */
#define	R_SETCURSOR	10		/* put (RECNO) */

typedef enum { DB_BTREE, DB_HASH, DB_RECNO } DBTYPE;

/* Access method description structure. */
typedef struct __db {
	DBTYPE type;			/* underlying db type */
	int (*close)	__P((struct __db *));
	int (*del)	__P((const struct __db *, const DBT *, u_int));
	int (*get)	__P((const struct __db *, const DBT *, DBT *, u_int));
	int (*put)	__P((const struct __db *, DBT *, const DBT *, u_int));
	int (*seq)	__P((const struct __db *, DBT *, DBT *, u_int));
	int (*sync)	__P((const struct __db *));
	void *internal;			/* access method private */
} DB;

#define	BTREEMAGIC	0x053162
#define	BTREEVERSION	3

/* Structure used to pass parameters to the btree routines. */
typedef struct {
#define	R_DUP		0x01	/* duplicate keys */
	u_long flags;
	int cachesize;		/* bytes to cache */
	int maxkeypage;		/* maximum keys per page */
	int minkeypage;		/* minimum keys per page */
	int psize;		/* page size */
				/* comparison, prefix functions */
	int (*compare)	__P((const DBT *, const DBT *));
	int (*prefix)	__P((const DBT *, const DBT *));
	int lorder;		/* byte order */
} BTREEINFO;

#define	HASHMAGIC	0x061561
#define	HASHVERSION	2

/* Structure used to pass parameters to the hashing routines. */
typedef struct {
	int bsize;		/* bucket size */
	int ffactor;		/* fill factor */
	int nelem;		/* number of elements */
	int cachesize;		/* bytes to cache */
	int (*hash)();		/* hash function */
	int lorder;		/* byte order */
} HASHINFO;

/* Structure used to pass parameters to the record routines. */
typedef struct {
#define	R_FIXEDLEN	0x01	/* fixed-length records */
#define	R_NOKEY		0x02	/* key not required */
#define	R_SNAPSHOT	0x04	/* snapshot the input */
	u_long flags;
	int cachesize;		/* bytes to cache */
	int lorder;		/* byte order */
	size_t reclen;		/* record length (fixed-length records) */
	u_char bval;		/* delimiting byte (variable-length records */
} RECNOINFO;

/* Key structure for the record routines. */
typedef struct {
	u_long number;
	u_long offset;
	u_long length;
#define	R_LENGTH	0x01	/* length is valid */
#define	R_NUMBER	0x02	/* record number is valid */
#define	R_OFFSET	0x04	/* offset is valid */
	u_char valid;
} RECNOKEY;

/*
 * Little endian <==> big endian long swap macros.
 *	BLSWAP		swap a memory location
 *	BLPSWAP		swap a referenced memory location
 *	BLSWAP_COPY	swap from one location to another
 */
#define BLSWAP(a) { \
	u_long _tmp = a; \
	((char *)&a)[0] = ((char *)&_tmp)[3]; \
	((char *)&a)[1] = ((char *)&_tmp)[2]; \
	((char *)&a)[2] = ((char *)&_tmp)[1]; \
	((char *)&a)[3] = ((char *)&_tmp)[0]; \
}
#define	BLPSWAP(a) { \
	u_long _tmp = *(u_long *)a; \
	((char *)a)[0] = ((char *)&_tmp)[3]; \
	((char *)a)[1] = ((char *)&_tmp)[2]; \
	((char *)a)[2] = ((char *)&_tmp)[1]; \
	((char *)a)[3] = ((char *)&_tmp)[0]; \
}
#define	BLSWAP_COPY(a, b) { \
	((char *)&(b))[0] = ((char *)&(a))[3]; \
	((char *)&(b))[1] = ((char *)&(a))[2]; \
	((char *)&(b))[2] = ((char *)&(a))[1]; \
	((char *)&(b))[3] = ((char *)&(a))[0]; \
}

/*
 * Little endian <==> big endian short swap macros.
 *	BSSWAP		swap a memory location
 *	BSPSWAP		swap a referenced memory location
 *	BSSWAP_COPY	swap from one location to another
 */
#define BSSWAP(a) { \
	u_short _tmp = a; \
	((char *)&a)[0] = ((char *)&_tmp)[1]; \
	((char *)&a)[1] = ((char *)&_tmp)[0]; \
}
#define BSPSWAP(a) { \
	u_short _tmp = *(u_short *)a; \
	((char *)a)[0] = ((char *)&_tmp)[1]; \
	((char *)a)[1] = ((char *)&_tmp)[0]; \
}
#define BSSWAP_COPY(a, b) { \
	((char *)&(b))[0] = ((char *)&(a))[1]; \
	((char *)&(b))[1] = ((char *)&(a))[0]; \
}

__BEGIN_DECLS
DB *dbopen __P((const char *, int, int, DBTYPE, const void *));

#ifdef __DBINTERFACE_PRIVATE
DB	*__bt_open __P((const char *, int, int, const BTREEINFO *));
DB	*__hash_open __P((const char *, int, int, const HASHINFO *));
DB	*__rec_open __P((const char *, int, int, const RECNOINFO *));
void	 __dbpanic __P((DB *dbp));
#endif
__END_DECLS
#endif /* !_DB_H_ */
