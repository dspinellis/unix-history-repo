/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)gettree.c	1.1 */

/*
 *   GETTREE.C
 *
 *   Programmer:  D. A. Lambeth
 *
 *        Owner:  D. A. Lambeth
 *
 *         Date:  April 17, 1980
 *
 *
 *
 *   GETTREE (MSIZE)
 *
 *        Create a shell associative memory with MSIZE buckets,
 *        and return a pointer to the root of the memory.
 *        MSIZE must be a power of 2.
 *
 *
 *
 *   See Also:  linknod(III), findnod(III), libname.h
 */

#include "name.h"
#include "flags.h"

/*
 *   GETTREE (MSIZE)
 *
 *      int MSIZE;
 *
 *   Create an associative memory containing MSIZE headnodes or
 *   buckets, and return a pointer to the root of the memory.
 *
 *   Algorithm:  Memory consists of a hash table of MSIZE buckets,
 *               each of which holds a pointer to a linked list
 *               of Namnods.  Nodes are hashed into a bucket by
 *               namid.
 */

extern char *malloc();

struct Amemory *gettree(msize)
register int msize;
{
	register struct Amemory *root;

	root = (struct Amemory *)malloc((unsigned)((msize-1)*sizeof(struct Namnod*) 
		+ sizeof(struct Amemory)));
	root->memsize = msize;
	root->nexttree = NULL;
	while (msize)
		root->memhead[--msize] = NULL;
	return (root);
}
