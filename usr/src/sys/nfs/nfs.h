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
 *	@(#)nfs.h	7.6 (Berkeley) %G%
 */

/*
 * Tunable constants for nfs
 */

#define	MAX_IOVEC	10
#define NFS_HZ		10		/* Ticks per second for NFS timeouts */
#define	NFS_TIMEO	(1*NFS_HZ)	/* Default timeout = 1 second */
#define	NFS_MINTIMEO	(NFS_HZ/2)	/* Min timeout to use */
#define	NFS_MAXTIMEO	(60*NFS_HZ)	/* Max timeout to backoff to */
#define	NFS_MAXREXMIT	100		/* Stop counting after this many */
#define	NFS_MAXWINDOW	1024		/* Max number of outstanding requests */
#define	NFS_RETRANS	10		/* Num of retrans for soft mounts */
#define NFS_FISHY	6		/* Host not responding at this count */
#define	NFS_ATTRTIMEO	5		/* Attribute cache timeout in sec */
#define	NFS_WSIZE	8192		/* Max. write data size <= 8192 */
#define	NFS_RSIZE	8192		/* Max. read data size <= 8192 */
#define	MAX_READDIR	NFS_RSIZE	/* Max. size of directory read */
#define	MAX_ASYNCDAEMON	20		/* Max. number async_daemons runnable */
#define	NMOD(a)		((a) % nfs_asyncdaemons)


/*
 * Nfs outstanding request list element
 */
struct nfsreq {
	struct nfsreq	*r_next;
	struct nfsreq	*r_prev;
	struct mbuf	*r_mreq;
	struct mbuf	*r_mrep;
	struct nfsmount *r_mntp;
	struct vnode	*r_vp;
	int		r_msiz;
	u_long		r_xid;
	short		r_flags;	/* flags on request, see below */
	short		r_retry;	/* max retransmission count */
	short		r_rexmit;	/* current retrans count */
	short		r_timer;	/* tick counter on reply */
	short		r_timerinit;	/* reinit tick counter on reply */
};

/* Flag values for r_flags */
#define R_TIMING	0x01		/* timing request (in mntp) */
#define R_SENT		0x02		/* request has been sent */

#ifdef	KERNEL
/*
 * Silly rename structure that hangs off the nfsnode until the name
 * can be removed by nfs_inactive()
 */
struct sillyrename {
	int	s_flag;
	nfsv2fh_t s_fh;
	struct nameidata s_namei;
};

/* And its flag values */
#define REMOVE		0
#define	RMDIR		1
#endif	/* KERNEL */

/*
 * Stats structure
 */
struct nfsstats {
	int	attrcache_hits;
	int	attrcache_misses;
	int	lookupcache_hits;
	int	lookupcache_misses;
	int	direofcache_hits;
	int	direofcache_misses;
	int	biocache_reads;
	int	read_bios;
	int	read_physios;
	int	biocache_writes;
	int	write_bios;
	int	write_physios;
	int	rpccnt[NFS_NPROCS];
	int	rpcretries;
	int	srvrpccnt[NFS_NPROCS];
	int	srvrpc_errs;
	int	srv_errs;
	int	rpcrequests;
	int	rpctimeouts;
	int	rpcunexpected;
	int	rpcinvalid;
	int	srvcache_inproghits;
	int	srvcache_idemdonehits;
	int	srvcache_nonidemdonehits;
	int	srvcache_misses;
};

#ifdef KERNEL
struct nfsstats nfsstats;
#endif /* KERNEL */
