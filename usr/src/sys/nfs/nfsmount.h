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
 *	@(#)nfsmount.h	7.5 (Berkeley) %G%
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
	struct	socket *nm_so;		/* Rpc socket */
	int	nm_sotype;		/* Type of socket */
	int	nm_soproto;		/* and protocol */
	int	nm_soflags;		/* pr_flags for socket protocol */
	struct	mbuf *nm_nam;		/* Addr of server */
	short	nm_retry;		/* Max retry count */
	short	nm_rexmit;		/* Rexmit on previous request */
	short	nm_rtt;			/* Round trip timer ticks @ NFS_HZ */
	short	nm_rto;			/* Current timeout */
	short	nm_srtt;		/* Smoothed round trip time */
	short	nm_rttvar;		/* RTT variance */
	short	nm_currto;		/* Current rto of any nfsmount */
	short	nm_currexmit;		/* Max rexmit count of nfsmounts */
	short	nm_sent;		/* Request send count */
	short	nm_window;		/* Request send window (max) */
	short	nm_winext;		/* Window incremental value */
	short	nm_ssthresh;		/* Slowstart threshold */
	short	nm_salen;		/* Actual length of nm_sockaddr */
	int	nm_rsize;		/* Max size of read rpc */
	int	nm_wsize;		/* Max size of write rpc */
};

#ifdef KERNEL
/*
 * Convert mount ptr to nfsmount ptr.
 */
#define VFSTONFS(mp)	((struct nfsmount *)((mp)->mnt_data))
#endif /* KERNEL */
