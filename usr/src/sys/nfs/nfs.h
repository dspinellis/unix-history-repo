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
 *	@(#)nfs.h	7.4 (Berkeley) %G%
 */

/*
 * Tunable constants for nfs
 */
#define	MAX_IOVEC	10
#define	NFS_TIMEO	10	/* Timeout in .1 sec intervals */
#define	NFS_MAXTIMEO	600	/* Max timeout to backoff too in .1 sec */
#define	NFS_ATTRTIMEO	5	/* Attribute cache timeout in sec */
#define	NFS_RETRANS	10	/* Num of retrans for soft mounts */
#define	NFS_WSIZE	8192	/* Max. write data size <= 8192 */
#define	NFS_RSIZE	8192	/* Max. read data size <= 8192 */
#define	MAX_READDIR	NFS_RSIZE	/* Max. size of directory read */
#define	MAX_ASYNCDAEMON	20	/* Max. number of async_daemons runnable */
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
	u_long		r_inaddr;
	u_long		r_retry;
	u_long		r_timeout;
	u_long		r_timer;
};

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

/*
 * Stats structure
 */
struct nfsstats {
	int	attrcache_hits;
	int	attrcache_misses;
	int	lookupcache_hits;
	int	lookupcache_misses;
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
	int	srvcache_inproghits;
	int	srvcache_idemdonehits;
	int	srvcache_nonidemdonehits;
	int	srvcache_misses;
};

#ifdef KERNEL
struct nfsstats nfsstats;
#endif
