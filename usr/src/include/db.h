/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)db.h	5.1 (Berkeley) %G%
 */

/* flags for DB.put() call */
#define	R_IBEFORE	1		/* RECNO */
#define	R_IAFTER	2		/* RECNO */
#define	R_NOOVERWRITE	3		/* BTREE, HASH, RECNO */
#define	R_PUT		4

/* flags for DB.seq() call */
#define	R_CURSOR	1		/* BTREE, RECNO */
#define	R_FIRST		2		/* BTREE, HASH, RECNO */
#define	R_LAST		3		/* BTREE, RECNO */
#define	R_NEXT		4		/* BTREE, HASH, RECNO */
#define	R_PREV		5		/* BTREE, RECNO */

/* key/data structure -- a data-base thang */
typedef struct {
	char *data;
	int size;
} DBT;

/* access method description structure */
typedef struct {
	char *internal;		/* access method private; really void * */
	int (*close)();
	int (*delete)();
	int (*get)();
	int (*put)();
	int (*seq)();
	int (*sync)();
} DB;

#define	BTREEMAGIC	0x053162

/* structure used to pass parameters to the btree routines */
typedef struct {
#define	R_DUP		0x01	/* duplicate keys */
	u_long flags;
	int cachesize;		/* bytes to cache */
	int psize;		/* page size */
	int (*compare)();	/* compare function */
} BTREEINFO;

#define	HASHMAGIC	0x061561

/* structure used to pass parameters to the hashing routines */
typedef struct {
	int bsize;		/* bucket size */
	int ffactor;		/* fill factor */
	int nelem;		/* number of elements */
	int ncached;		/* bytes to cache */
	int (*hash)();		/* hash function */
} HASHINFO;

/* structure used to pass parameters to the record routines */
typedef struct {
#define	R_FIXEDLEN	0x01	/* fixed-length records */
	u_long flags;
	int cachesize;		/* bytes to cache */
	size_t reclen;		/* record length (fixed-length records) */
	u_char bval;		/* delimiting byte (variable-length records */
} RECNOINFO;

/* key structure for the record routines */
typedef struct {
	u_long number;
	u_long offset;
	u_long length;
#define	R_LENGTH	0x01	/* length is valid */
#define	R_NUMBER	0x02	/* record number is valid */
#define	R_OFFSET	0x04	/* offset is valid */
	u_char valid;
} RECNOKEY;

#if __STDC__ || c_plusplus
DB *btree_open(const char *file, int flags, int mode, const BTREEINFO *private);
DB *hash_open(const char *file, int flags, int mode, const HASHINFO *private);
DB *recno_open(const char *file, int flags, int mode, const RECNOINFO *private);
#else
DB *btree_open();
DB *hash_open();
DB *recno_open();
#endif
