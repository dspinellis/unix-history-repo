/*
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)bt_subr.c	8.2 (Berkeley) %G%
 *
 * from: $Header: bt_subr.c,v 1.1 93/10/12 15:28:39 torek Exp $
 */

#include <sys/param.h>
#include <sys/buf.h>
#include <sys/errno.h>
#include <sys/fbio.h>

#include <sparc/sbus/btreg.h>
#include <sparc/sbus/btvar.h>

/*
 * Common code for dealing with Brooktree video DACs.
 * (Contains some software-only code as well, since the colormap
 * ioctls are shared between the cgthree and cgsix drivers.)
 */

/*
 * Implement an FBIOGETCMAP-like ioctl.
 */
int
bt_getcmap(p, cm, cmsize)
	register struct fbcmap *p;
	union bt_cmap *cm;
	int cmsize;
{
	register u_int i, start, count;
	register u_char *cp;

	start = p->index;
	count = p->count;
	if (start >= cmsize || start + count > cmsize)
		return (EINVAL);
	if (!useracc(p->red, count, B_WRITE) ||
	    !useracc(p->green, count, B_WRITE) ||
	    !useracc(p->blue, count, B_WRITE))
		return (EFAULT);
	for (cp = &cm->cm_map[start][0], i = 0; i < count; cp += 3, i++) {
		p->red[i] = cp[0];
		p->green[i] = cp[1];
		p->blue[i] = cp[2];
	}
	return (0);
}

/*
 * Implement the software portion of an FBIOPUTCMAP-like ioctl.
 */
int
bt_putcmap(p, cm, cmsize)
	register struct fbcmap *p;
	union bt_cmap *cm;
	int cmsize;
{
	register u_int i, start, count;
	register u_char *cp;

	start = p->index;
	count = p->count;
	if (start >= cmsize || start + count > cmsize)
		return (EINVAL);
	if (!useracc(p->red, count, B_READ) ||
	    !useracc(p->green, count, B_READ) ||
	    !useracc(p->blue, count, B_READ))
		return (EFAULT);
	for (cp = &cm->cm_map[start][0], i = 0; i < count; cp += 3, i++) {
		cp[0] = p->red[i];
		cp[1] = p->green[i];
		cp[2] = p->blue[i];
	}
	return (0);
}
