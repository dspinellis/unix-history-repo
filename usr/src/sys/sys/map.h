/*-
 * Copyright (c) 1982, 1986, 1993
 *	The Regents of the University of California.  All rights reserved.
 * (c) UNIX System Laboratories, Inc.
 * All or some portions of this file are derived from material licensed
 * to the University of California by American Telephone and Telegraph
 * Co. or Unix System Laboratories, Inc. and are reproduced herein with
 * the permission of UNIX System Laboratories, Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)map.h	8.2 (Berkeley) %G%
 */

/*
 * Resource allocation maps.
 *
 * Associated routines manage sub-allocation of an address space using
 * an array of segment descriptors.  The first element of this array
 * is a map structure, describing the arrays extent and the name
 * of the controlled object.  Each additional structure represents
 * a free segment of the address space.
 *
 * A call to rminit initializes a resource map and may also be used
 * to free some address space for the map.  Subsequent calls to rmalloc
 * and rmfree allocate and free space in the resource map.  If the resource
 * map becomes too fragmented to be described in the available space,
 * then some of the resource is discarded.  This may lead to critical
 * shortages, but is better than not checking (as the previous versions
 * of these routines did) or giving up and calling panic().  The routines
 * could use linked lists and call a memory allocator when they run
 * out of space, but that would not solve the out of space problem when
 * called at interrupt time.
 *
 * N.B.: The address 0 in the resource address space is not available
 * as it is used internally by the resource map routines.
 */
struct map {
	struct	mapent *m_limit;	/* address of last slot in map */
	char	*m_name;		/* name of resource, for messages */
};

struct mapent {
	int	m_size;			/* size of this segment of the map */
	int	m_addr;			/* start of segment */
};

#ifdef KERNEL
#define	ARGMAPSIZE	16
struct	map *kmemmap, *mbmap, *swapmap;
int	nswapmap;

long	rmalloc __P((struct map *, long));
void	rmfree __P((struct map *, long, long));
void	rminit __P((struct map *, long, long, char *, int));
#endif
