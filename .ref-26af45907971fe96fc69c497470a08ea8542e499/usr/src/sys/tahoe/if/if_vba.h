/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)if_vba.h	1.2 (Berkeley) %G%
 */

struct ifvba {
	struct	mbuf *iff_mbuf;	/* associated mbuf to free */
	caddr_t	iff_buffer;	/* contiguous memory for data, kernel address */
	u_long	iff_physaddr;	/* contiguous memory for data, phys address */
};

#define VIFF_16BIT 1		/* only allow two byte transfers */

#ifdef KERNEL
struct mbuf *if_vbaget();
#endif
