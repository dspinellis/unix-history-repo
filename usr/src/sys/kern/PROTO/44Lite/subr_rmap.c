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

rmfree(mp, size, addr)
	struct map *mp;
	long size, addr;
{

	/*
	 * Body deleted.
	 */
	return;
}
