/*
 * This source code is a product of Sun Microsystems, Inc. and is provided
 * for unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify this source code without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 *
 * THIS PROGRAM CONTAINS SOURCE CODE COPYRIGHTED BY SUN MICROSYSTEMS, INC.
 * SUN MICROSYSTEMS, INC., MAKES NO REPRESENTATIONS ABOUT THE SUITABLITY
 * OF SUCH SOURCE CODE FOR ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT
 * EXPRESS OR IMPLIED WARRANTY OF ANY KIND.  SUN MICROSYSTEMS, INC. DISCLAIMS
 * ALL WARRANTIES WITH REGARD TO SUCH SOURCE CODE, INCLUDING ALL IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN
 * NO EVENT SHALL SUN MICROSYSTEMS, INC. BE LIABLE FOR ANY SPECIAL, INDIRECT,
 * INCIDENTAL, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING
 * FROM USE OF SUCH SOURCE CODE, REGARDLESS OF THE THEORY OF LIABILITY.
 * 
 * This source code is provided with no support and without any obligation on
 * the part of Sun Microsystems, Inc. to assist in its use, correction, 
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS
 * SOURCE CODE OR ANY PART THEREOF.
 *
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California 94043
 */

/* @(#)cache.h 1.4 69/12/31 SMI */

/*
 * ld.so directory caching
 */

/*
 * Copyright (c) 1987, 1991 by Sun Microsystems, Inc.
 */

/*
 * Shared object lookup performance in the run-time link editor is 
 * enhanced through the use of caches for directories that the editor
 * searches.  A given "cache" describes the contents of a single directory,
 * and each cache entry contains the canonical name for a shared object
 * as well as its absolute pathname.
 *
 * Within a cache, "pointers" are really relative addresses to some absolute
 * address (often the base address of the containing database).
 */

#define	CACHE_FILE	"/etc/ld.so.cache"	/* file where it is stored */

/*
 * Relative pointer macros.
 */
#define	RELPTR(base, absptr) ((long)(absptr) - (long)(base))
#define	AP(base) ((caddr_t)base)

/*
 * Definitions for cache structures.
 */
#define DB_HASH		11		/* number of hash buckets in caches */
#define LD_CACHE_MAGIC 	0x041155	/* cookie to identify data structure */
#define LD_CACHE_VERSION 0		/* version number of cache structure*/

struct	dbe	{			/* element of a directory cache */
	long	dbe_next;		/* (rp) next element on this list */
	long	dbe_lop;		/* (rp) canonical name for object */
	long	dbe_name;		/* (rp) absolute name */
};

struct	db	{			/* directory cache database */
	long	db_name;		/* (rp) directory contained here */
	struct	dbe db_hash[DB_HASH];	/* hash buckets */
	caddr_t	db_chain;		/* private to database mapping */
};

struct dbf 	{			/* cache file image */
	long dbf_magic;			/* identifying cookie */
	long dbf_version;		/* version no. of these dbs */
	long dbf_machtype;		/* machine type */
	long dbf_db;		/* directory cache dbs */
};

/*
 * Structures used to describe and access a database.
 */
struct	dbd	{			/* data base descriptor */
	struct	dbd *dbd_next;		/* next one on this list */
	struct	db *dbd_db;		/* data base described by this */
};

struct	dd	{			/* directory descriptor */
	struct	dd *dd_next;		/* next one on this list */
	struct	db *dd_db;		/* data base described by this */
};

/*
 * Interfaces imported/exported by the lookup code.
 */
caddr_t (*db_malloc)();			/* allocator for relative objects */
caddr_t (*heap_malloc)();		/* allocator for general objects */
int (*is_secure)();			/* tells whether censorship in force */
extern	int use_cache;			/* use existing cache? */
extern caddr_t db_base;

char	*lo_lookup(/* lop, lmp */);	/* name for link_object */
struct	db *lo_cache(/* cp */);		/* obtain cache for directory name */
void	lo_flush();			/* foreach dir get a new database */
void	dbd_flush();			/* delete the dbs that came from 
					   ld.so.cache */
