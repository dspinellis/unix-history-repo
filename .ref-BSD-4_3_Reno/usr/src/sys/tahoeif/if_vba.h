/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)if_vba.h	1.2 (Berkeley) 6/28/90
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
