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

/* @(#)ldconfig.c 1.10 91/03/17 SMI */

/*
 * Copyright (c) 1991 by Sun Microsystems, Inc. */
 */

/*
 * ldconfig: build configuration information for ld.so.
 */

#include <stdio.h>			/* I/O */
#include <sys/mman.h>			/* memory management */
#include <sys/param.h>			/* general system definitions */
#include <sys/file.h>			/* file I/O bits */
#include <sys/exec.h>
#include <link.h>			/* link editor public */
#include "cache.h"			/* local caching information */

/*
 * Global storage and definitions.
 */
#define	MAX_DB	(1024 * 1024)		/* 1Mb database is the max */

int	fd;				/* descriptor on database */
caddr_t	db_base;			/* base address of last database */
caddr_t	heap_ptr;			/* current file allocation */
caddr_t heap_base;			/* base address of file mapping */
caddr_t heap_max;			/* maximum file to allocate */
void build_cache();			/* build a cache given directory */
int	first = 1;			/* true if no first entry yet */
struct	dbf *dbfp;			/* working cache file pointer */

/*
 * "Environmental" variables exported to the cache routines.
 */
extern	caddr_t malloc();		/* heap allocator */
caddr_t	mmap_alloc();			/* database allocator */
int	false();			/* says we're never secure */
int	use_cache = 0;			/* no previous database */

/*
 * List of directories which are always in the cache.  Note: these need
 * not be (nor are they) identical to the directories searched by default
 * by the loader.
 */
char *default_directories[] = {"/usr/lib", "/usr/5lib", 
	"/usr/lib/fsoft", "/usr/lib/f68881", "/usr/lib/ffpa",
	"/usr/lib/fswitch", 0};

/*
 * Build a database by building over all arguments.
 */
main(argc, argv)
	int argc;
	char *argv[];
{
	int i;				/* loop temporary */
	char *fnp;			/* temporary file name */
	char **cpp;			/* loop temporary */

	/*
	 * Initialize exported functions.
	 */
	db_malloc = mmap_alloc;
	heap_malloc = malloc;
	is_secure = false;

	/*
	 * Make a temporary file, map it in up to its maximum length.
	 */
	fnp = (char *)mktemp("/etc/ld.so.XXXXXX");
	if ((fd = open(fnp, O_CREAT|O_RDWR, 0644)) == -1) {
		perror("ldconfig: open temporary");
		exit(1);
	}
	if ((heap_base = mmap(0, MAX_DB, PROT_READ|PROT_WRITE, 
	    MAP_SHARED, fd, 0)) == (caddr_t)-1) {
		perror("ldconfig: mmap");
		exit(1);
	}
	db_base = heap_ptr = heap_max = heap_base;
	dbfp = (struct dbf *)(*db_malloc)(sizeof (struct dbf));
	dbfp->dbf_magic = LD_CACHE_MAGIC;
#if TARGET==SUN2
	dbfp->dbf_machtype = M_68010;
#endif
#if TARGET==SUN3
	dbfp->dbf_machtype = M_68020;
#endif
#if TARGET==SUN4
	dbfp->dbf_machtype = M_SPARC;
#endif
	dbfp->dbf_version = LD_CACHE_VERSION;

	/*
	 * For each argument, create a database.  
	 */
	for (i = 1; i < argc; i++) 
		build_cache(argv[i], 1);

	/*
	 * Build a database for each of the default directories.
	 * Note that it is not an error for these directories to
	 * fail (they may not exist in all installations).
	 */
	for (cpp = default_directories; *cpp; cpp++)
		build_cache(*cpp, 0);

	/*
	 * Database built.  Close file, and rename it to be the "real"
	 * database.  However, if nothing every actually got cached,
	 * delete it instead.
	 */
	(void) close(fd);
	if (first) {
		(void) unlink(fnp);
		exit(0);
	}
	if (rename(fnp, CACHE_FILE) == -1) {
		perror("ldconfig: rename");
		exit(1);
	}
	exit(0);
	/*NOTREACHED*/	
}

/*
 * Allocator for mmap arena.
 */
caddr_t
mmap_alloc(n)
	int n;
{
	caddr_t a;
	static u_int page_size = 0;

	/*
	 * Get value -- simply next address to allocate.
	 */
	a = heap_ptr;

	/*
	 * Architecture-dependent rounding.
	 */
#ifdef sparc
	n = round(n, sizeof (double));
#endif sparc
#ifdef mc68000
	n = round(n, sizeof (int));
#endif mc68000

	/*
	 * If block to be allocated requires file extension, do so.
	 */
	if ((heap_ptr + n) >= heap_max) {
		if (page_size == 0)
			page_size = getpagesize();
		heap_max = (caddr_t)roundup((u_int)heap_ptr + n, page_size);
		if ((heap_max - heap_base) > MAX_DB) {
			perror("ldconfig: data base overflow");
			exit(1);
		}
		if (ftruncate(fd, heap_max - heap_base) == -1) {
			perror("ldconfig: ftruncate");
			exit(1);
		}
	}

	/*
	 * Allocate full block, return starting address.
	 */
	heap_ptr += n;
	return (a);
}

/*
 * Call support routines to build a cache for the given directory.
 */
void
build_cache(cp, flag_error)
	char *cp;			/* directory to cache */
	int flag_error;			/* true if errors to be flagged */
{
	struct db *dbp;			/* working data base file */

	if ((dbp = lo_cache(cp)) == (struct db *)NULL) {
		if (flag_error)
			(void) fprintf(stderr,
			    "ldconfig: directory %s not cached\n", cp);
		return;
	}
	if (first) {
		dbfp->dbf_db = RELPTR(dbfp, dbp);
		db_base = (caddr_t) dbp = &AP(dbfp)[(int)dbfp->dbf_db];
		first = 0;
	}
	if (dbp->db_chain ==  0) {
		dbp->db_chain = (caddr_t)(heap_ptr - db_base);
		db_base = heap_ptr;
	}
}

/*
 * Dummy test to say we're never using security checks.
 */
int
false()
{

	return (0);
}

/*
 * Rounding function.
 */
round(v, r)
	int v;				/* value to be rounded */
	int r;				/* rounding point */
{

	v += --r;
	v &= ~r;
	return (v);
}
