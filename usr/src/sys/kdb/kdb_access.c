/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kdb_access.c	7.4 (Berkeley) 5/3/90
 */

/*
 * Access data in kernel address space.
 */

#include "../kdb/defs.h"

/*
 * Primitives: put a value in a space, get a value from a space
 * and get a word or byte not returning if an error occurred.
 */
/*ARGSUSED*/
kdbput(addr, space, value) 
    off_t addr; long value; { (void) kdbpoke((caddr_t)addr, value); }

/*ARGSUSED*/
u_int
kdbget(addr, space)
    off_t addr; { return (kdbpeek((caddr_t)addr)); };

u_int
kdbchkget(addr, space)
    off_t addr; { u_int w = kdbget(addr, space); kdbchkerr(); return (w); }

u_int
kdbbchkget(addr, space) 
    off_t addr; { return (byte(kdbchkget(addr, space))); }
