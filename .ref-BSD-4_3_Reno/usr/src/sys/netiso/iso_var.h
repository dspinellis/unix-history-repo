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
 * Modifications,
 * Copyright (c) 1988 Regents of the University of California.
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
 *	@(#)iso_var.h	7.4 (Berkeley) 6/28/90
 */

/*
 * ARGO Project, Computer Sciences Dept., University of Wisconsin - Madison
 */
/* $Header: iso_var.h,v 4.2 88/06/29 15:00:08 hagens Exp $
 * $Source: /usr/argo/sys/netiso/RCS/iso_var.h,v $
 */

/*
 *	Interface address, iso version. One of these structures is 
 *	allocated for each interface with an osi address. The ifaddr
 *	structure conatins the protocol-independent part
 *	of the structure, and is assumed to be first.
 */
struct iso_ifaddr {
	struct ifaddr		ia_ifa;		/* protocol-independent info */
#define ia_ifp		ia_ifa.ifa_ifp
#define	ia_flags	ia_ifa.ifa_flags
	int					ia_snpaoffset;
	struct iso_ifaddr	*ia_next;	/* next in list of iso addresses */
	struct	sockaddr_iso ia_addr;	/* reserve space for interface name */
	struct	sockaddr_iso ia_dstaddr; /* reserve space for broadcast addr */
#define	ia_broadaddr	ia_dstaddr
	struct	sockaddr_iso ia_sockmask; /* reserve space for general netmask */
};

struct	iso_aliasreq {
	char	ifra_name[IFNAMSIZ];		/* if name, e.g. "en0" */
	struct	sockaddr_iso ifra_addr;
	struct	sockaddr_iso ifra_dstaddr;
	struct	sockaddr_iso ifra_mask;
	int	ifra_snpaoffset;
};

struct	iso_ifreq {
	char	ifr_name[IFNAMSIZ];		/* if name, e.g. "en0" */
	struct	sockaddr_iso ifr_Addr;
};

/*
 *	Given a pointer to an iso_ifaddr (ifaddr),
 *	return a pointer to the addr as a sockaddr_iso
 */
/*
#define	IA_SIS(ia) ((struct sockaddr_iso *)(ia.ia_ifa->ifa_addr))
 * works if sockaddr_iso becomes variable sized.
 */
#define	IA_SIS(ia) (&(((struct iso_ifaddr *)ia)->ia_addr))

#define	SIOCDIFADDR_ISO	_IOW('i',25, struct iso_ifreq)	/* delete IF addr */
#define	SIOCAIFADDR_ISO	_IOW('i',26, struct iso_aliasreq)/* add/chg IFalias */
#define	SIOCGIFADDR_ISO	_IOWR('i',33, struct iso_ifreq)	/* get ifnet address */
#define	SIOCGIFDSTADDR_ISO _IOWR('i',34, struct iso_ifreq) /* get dst address */
#define	SIOCGIFNETMASK_ISO _IOWR('i',37, struct iso_ifreq) /* get dst address */

/*
 * This stuff should go in if.h or if_llc.h or someplace else,
 * but for now . . .
 */

struct llc_etherhdr {
	char dst[6];
	char src[6];
	char len[2];
	char llc_dsap;
	char llc_ssap;
	char llc_ui_byte;
};

struct snpa_hdr {
	struct	ifnet *snh_ifp;
	char	snh_dhost[6];
	char	snh_shost[6];
	short	snh_flags;
};
#ifdef KERNEL
struct iso_ifaddr	*iso_ifaddr;	/* linked list of iso address ifaces */
struct iso_ifaddr	*iso_localifa();	/* linked list of iso address ifaces */
struct ifqueue 		clnlintrq;		/* clnl packet input queue */
#endif KERNEL
