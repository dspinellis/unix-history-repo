/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)nfsmount.h	7.4 (Berkeley) %G%
 */

/*
 * Mount structure.
 * One allocated on every NFS mount.
 * Holds NFS specific information for mount.
 */
struct	nfsmount {
	int	nm_flag;		/* Flags for soft/hard... */
	struct	mount *nm_mountp;	/* Vfs structure for this filesystem */
	nfsv2fh_t nm_fh;		/* File handle of root dir */
	struct	socket	*nm_so;		/* Rpc socket */
	struct	nfshost *nm_hostinfo;	/* Host and congestion information */
	short	nm_retry;		/* Max retry count */
	short	nm_rexmit;		/* Rexmit on previous request */
	short	nm_rtt;			/* Round trip timer ticks @ NFS_HZ */
	short	nm_rto;			/* Current timeout */
	short	nm_srtt;		/* Smoothed round trip time */
	short	nm_rttvar;		/* RTT variance */
	int	nm_rsize;		/* Max size of read rpc */
	int	nm_wsize;		/* Max size of write rpc */
};

/*
 * Hostinfo/congestion structure.
 * One allocated per NFS server.
 * Holds host address, congestion limits, request count, etc.
 * Reference count is of nfsmounts which point to it.
 */
struct nfshost {
	struct	nfshost *nh_next, *nh_prev;
	short	nh_refcnt;		/* Reference count */
	short	nh_currto;		/* Current rto of any nfsmount */
	short	nh_currexmit;		/* Max rexmit count of nfsmounts */
	short	nh_sent;		/* Request send count */
	short	nh_window;		/* Request send window (max) */
	short	nh_winext;		/* Window incremental value */
	short	nh_ssthresh;		/* Slowstart threshold */
	short	nh_salen;		/* Actual length of nh_sockaddr */
	struct	mbuf *nh_sockaddr;	/* Address of server */
};

#ifdef KERNEL
/*
 * Convert mount ptr to nfsmount ptr.
 */
#define VFSTONFS(mp)	((struct nfsmount *)((mp)->mnt_data))
#endif /* KERNEL */
