/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	from: @(#)subr_rmap.c	7.9 (Berkeley) 5/11/91
 */

#include "param.h"
#include "systm.h"
#include "map.h"
#include "dmap.h"		/* XXX */
#include "proc.h"
#include "kernel.h"

/*
 * Resource map handling routines.
 *
 * A resource map is an array of structures each
 * of which describes a segment of the address space of an available
 * resource.  The segments are described by their base address and
 * length, and sorted in address order.  Each resource map has a fixed
 * maximum number of segments allowed.  Resources are allocated
 * by taking part or all of one of the segments of the map.
 *
 * Returning of resources will require another segment if
 * the returned resources are not adjacent in the address
 * space to an existing segment.  If the return of a segment
 * would require a slot which is not available, then one of
 * the resource map segments is discarded after a warning is printed.
 * Returning of resources may also cause the map to collapse
 * by coalescing two existing segments and the returned space
 * into a single segment.  In this case the resource map is
 * made smaller by copying together to fill the resultant gap.
 *
 * N.B.: the current implementation uses a dense array and does
 * not admit the value ``0'' as a legal address, since that is used
 * as a delimiter.
 */

/*
 * Initialize map mp to have (mapsize-2) segments
 * and to be called ``name'', which we print if
 * the slots become so fragmented that we lose space.
 * The map itself is initialized with size elements free
 * starting at addr.
 */
rminit(mp, size, addr, name, mapsize)
	register struct map *mp;
	long size, addr;
	char *name;
	int mapsize;
{

	/*
	 * Body deleted.
	 */
	return;
}

/*
 * A piece of memory of at least size units is allocated from the
 * specified map using a first-fit algorithm. It returns the starting
 * address of the allocated space.
 *
 * This routine knows about and handles the interleaving of the swapmap.
 */
long
rmalloc(mp, size)
	register struct map *mp;
	long size;
{

	/*
	 * Body deleted.
	 */
	return (0);
}

/*
 * The previously allocated space at addr of size units is freed
 * into the specified map. This routine is responsible for sorting
 * the frred space into the correct location in the map, and coalescing
 * it with free space on either side if they adjoin.
 */
rmfree(mp, size, addr)
	struct map *mp;
	long size, addr;
{

	/*
	 * Body deleted.
	 */
	return;
}
