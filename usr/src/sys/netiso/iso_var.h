/***********************************************************
		Copyright IBM Corporation 1987

                      All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of IBM not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

IBM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
IBM BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/*
 * ARGO Project, Computer Sciences Dept., University of Wisconsin - Madison
 */
/* $Header: iso_var.h,v 4.2 88/06/29 15:00:08 hagens Exp $ */
/* $Source: /usr/argo/sys/netiso/RCS/iso_var.h,v $ */

/*
 *	Interface address, iso version. One of these structures is 
 *	allocated for each interface with an osi address. The ifaddr
 *	structure conatins the protocol-independent part
 *	of the structure, and is assumed to be first.
 */
struct iso_ifaddr {
	struct ifaddr		ia_ifa;		/* protocol-independent info */
	int					ia_flags;
	struct iso_ifaddr	*ia_next;	/* next in list of iso addresses */
};
#define ia_ifp		ia_ifa.ifa_ifp
#ifndef ia_addr
#define ia_addr		ia_ifa.ifa_addr
#endif	ia_addr

/*
 *	Given a pointer to an iso_ifaddr (ifaddr),
 *	return a pointer to the addr as a sockaddr_iso
 */
#define	IA_SIS(ia)\
	((struct sockaddr_iso *)(&((struct iso_ifaddr *)ia)->ia_addr))

#ifndef	IFA_ROUTE
#define	IFA_ROUTE	0x01			/* routing entry installed */
#endif	IFA_ROUTE

#ifdef KERNEL
struct iso_ifaddr	*iso_ifaddr;	/* linked list of iso address ifaces */
struct ifqueue 		clnlintrq;		/* clnl packet input queue */
#endif KERNEL
