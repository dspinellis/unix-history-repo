/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: if_news.h,v 4.300 91/06/09 06:26:02 root Rel41 $ SONY
 *
 *	@(#)if_news.h	7.1 (Berkeley) %G%
 */

/*
 * Structure and routine definitions
 * for NEWS network interfaces.
 */

/*
 * Each interface has one of these structures giving information
 * about NEWS resources held by the interface.
 */
struct ifnews {
	int	ifn_hlen;			/* local net header length */
	caddr_t	ifn_raddr;			/* receive buffer address */
	caddr_t	ifn_waddr;			/* transmit buffer address */
	struct	mbuf *ifn_mbuf;			/* packet being transmitted */
};

struct mbuf_segment {
	u_int	ms_physaddr;
	u_int	ms_size;
};

#ifdef KERNEL
struct mbuf *if_rnewsget();
#endif

#define	LARGE_DATA 	512
