/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
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
 *	@(#)if_uba.h	7.4 (Berkeley) 6/28/90
 */

/*
 * Structure and routine definitions
 * for UNIBUS network interfaces.
 */

#define	IF_MAXNUBAMR	10
/*
 * Each interface has structures giving information
 * about UNIBUS resources held by the interface
 * for each send and receive buffer.
 *
 * We hold IF_NUBAMR map registers for datagram data, starting
 * at ifr_mr.  Map register ifr_mr[-1] maps the local network header
 * ending on the page boundary.  Bdp's are reserved for read and for
 * write, given by ifr_bdp.  The prototype of the map register for
 * read and for write is saved in ifr_proto.
 *
 * When write transfers are not full pages on page boundaries we just
 * copy the data into the pages mapped on the UNIBUS and start the
 * transfer.  If a write transfer is of a (1024 byte) page on a page
 * boundary, we swap in UNIBUS pte's to reference the pages, and then
 * remap the initial pages (from ifu_wmap) when the transfer completes.
 *
 * When read transfers give whole pages of data to be input, we
 * allocate page frames from a network page list and trade them
 * with the pages already containing the data, mapping the allocated
 * pages to replace the input pages for the next UNIBUS data input.
 */

/*
 * Information per interface.
 */
struct	ifubinfo {
	short	iff_uban;			/* uba number */
	short	iff_hlen;			/* local net header length */
	struct	uba_regs *iff_uba;		/* uba adaptor regs, in vm */
	struct	pte *iff_ubamr;			/* uba map regs, in vm */
	short	iff_flags;			/* used during uballoc's */
};

/*
 * Information per buffer.
 */
struct ifrw {
	caddr_t	ifrw_addr;			/* virt addr of header */
	short	ifrw_bdp;			/* unibus bdp */
	short	ifrw_flags;			/* type, etc. */
#define	IFRW_W	0x01				/* is a transmit buffer */
	int	ifrw_info;			/* value from ubaalloc */
	int	ifrw_proto;			/* map register prototype */
	struct	pte *ifrw_mr;			/* base of map registers */
};

/*
 * Information per transmit buffer, including the above.
 */
struct ifxmt {
	struct	ifrw ifrw;
	caddr_t	ifw_base;			/* virt addr of buffer */
	struct	pte ifw_wmap[IF_MAXNUBAMR];	/* base pages for output */
	struct	mbuf *ifw_xtofree;		/* pages being dma'd out */
	short	ifw_xswapd;			/* mask of clusters swapped */
	short	ifw_nmr;			/* number of entries in wmap */
};
#define	ifw_addr	ifrw.ifrw_addr
#define	ifw_bdp		ifrw.ifrw_bdp
#define	ifw_flags	ifrw.ifrw_flags
#define	ifw_info	ifrw.ifrw_info
#define	ifw_proto	ifrw.ifrw_proto
#define	ifw_mr		ifrw.ifrw_mr

/*
 * Most interfaces have a single receive and a single transmit buffer,
 * and use struct ifuba to store all of the unibus information.
 */
struct ifuba {
	struct	ifubinfo ifu_info;
	struct	ifrw ifu_r;
	struct	ifxmt ifu_xmt;
};

#define	ifu_uban	ifu_info.iff_uban
#define	ifu_hlen	ifu_info.iff_hlen
#define	ifu_uba		ifu_info.iff_uba
#define	ifu_ubamr	ifu_info.iff_ubamr
#define	ifu_flags	ifu_info.iff_flags
#define	ifu_w		ifu_xmt.ifrw
#define	ifu_xtofree	ifu_xmt.ifw_xtofree

#ifdef 	KERNEL
#define	if_ubainit(ifuba, uban, hlen, nmr) \
		if_ubaminit(&(ifuba)->ifu_info, uban, hlen, nmr, \
			&(ifuba)->ifu_r, 1, &(ifuba)->ifu_xmt, 1)
#define	if_rubaget(ifu, totlen, off0, ifp) \
		if_ubaget(&(ifu)->ifu_info, &(ifu)->ifu_r, totlen, off0, ifp)
#define	if_wubaput(ifu, m) \
		if_ubaput(&(ifu)->ifu_info, &(ifu)->ifu_xmt, m)
struct	mbuf *if_ubaget();
#endif
