/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
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
 *
 *	@(#)db.h	5.10 (Berkeley) 4/2/91
 */

#ifndef _DB_H_
#define	_DB_H_

#include <sys/cdefs.h>

/* flags for DB.put() call */
#define	R_IBEFORE	1		/* RECNO */
#define	R_IAFTER	2		/* RECNO */
#define	R_NOOVERWRITE	3		/* BTREE, HASH, RECNO */
#define	R_PUT		4		/* BTREE, HASH, RECNO */

/* flags for DB.seq() call */
#define	R_CURSOR	1		/* BTREE, RECNO */
#define	R_FIRST		2		/* BTREE, HASH, RECNO */
#define	R_LAST		3		/* BTREE, RECNO */
#define	R_NEXT		4		/* BTREE, HASH, RECNO */
#define	R_PREV		5		/* BTREE, RECNO */

/* key/data structure -- a data-base thang */
typedef struct {
	void *data;
	int size;
} DBT;

/* access method description structure */
typedef struct __db {
	void *internal;		/* access method private */
#define	DB_BTREE	1
#define	DB_HASH		2
#define	DB_RECNO	3
	int type;		/* type of underlying db */
	int (*close) __P((const struct __db *));
	int (*del) __P((const struct __db *, const DBT *, unsigned int));
	int (*get) __P((const struct __db *, DBT *, DBT *, unsigned int));
	int (*put) __P((const struct __db *, const DBT *, const DBT *,
		unsigned int));
	int (*seq) __P((const struct __db *, DBT *, DBT *, unsigned int));
	int (*sync) __P((const struct __db *));
} DB;

#define	BTREEMAGIC	0x053162
#define	BTREEVERSION	2

/* structure used to pass parameters to the btree routines */
typedef struct {
#define	R_DUP		0x01	/* duplicate keys */
	u_long flags;
	int cachesize;		/* bytes to cache */
	int psize;		/* page size */
	int (*compare)();	/* compare function */
	int lorder;		/* byte order */
} BTREEINFO;

#define	HASHMAGIC	0x061561
#define	HASHVERSION	1

/* structure used to pass parameters to the hashing routines */
typedef struct {
	int bsize;		/* bucket size */
	int ffactor;		/* fill factor */
	int nelem;		/* number of elements */
	int cachesize;		/* bytes to cache */
	int (*hash)();		/* hash function */
	int lorder;		/* byte order */
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

/* Little endian <--> big endian long swap macros. */
#define BLSWAP(a) { \
	u_long _tmp = a; \
	((char *)&a)[0] = ((char *)&_tmp)[3]; \
	((char *)&a)[1] = ((char *)&_tmp)[2]; \
	((char *)&a)[2] = ((char *)&_tmp)[1]; \
	((char *)&a)[3] = ((char *)&_tmp)[0]; \
}
#define	BLSWAP_COPY(a,b) { \
	((char *)&(b))[0] = ((char *)&(a))[3]; \
	((char *)&(b))[1] = ((char *)&(a))[2]; \
	((char *)&(b))[2] = ((char *)&(a))[1]; \
	((char *)&(b))[3] = ((char *)&(a))[0]; \
}


/* Little endian <--> big endian short swap macros. */
#define BSSWAP(a) { \
	u_short _tmp = a; \
	((char *)&a)[0] = ((char *)&_tmp)[1]; \
	((char *)&a)[1] = ((char *)&_tmp)[0]; \
}
#define BSSWAP_COPY(a,b) { \
	((char *)&(b))[0] = ((char *)&(a))[1]; \
	((char *)&(b))[1] = ((char *)&(a))[0]; \
}

__BEGIN_DECLS
DB	*btree_open
	    __P((const char *, int, int, const BTREEINFO *));
DB	*hash_open
	    __P((const char *, int, int, const HASHINFO *));
DB	*recno_open
	    __P((const char *, int, int, const RECNOINFO *));
__END_DECLS

#endif /* !_DB_H_ */
