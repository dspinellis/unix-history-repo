/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
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
 *	@(#)nfs.h	7.9 (Berkeley) 6/28/90
 */

/*
 * Tunable constants for nfs
 */

#define	NFS_MAXIOVEC	34
#define NFS_HZ		10		/* Ticks per second for NFS timeouts */
#define	NFS_TIMEO	(1*NFS_HZ)	/* Default timeout = 1 second */
#define	NFS_MINTIMEO	(NFS_HZ)	/* Min timeout to use */
#define	NFS_MAXTIMEO	(60*NFS_HZ)	/* Max timeout to backoff to */
#define	NFS_MINIDEMTIMEO (2*NFS_HZ)	/* Min timeout for non-idempotent ops*/
#define	NFS_RELIABLETIMEO (5*NFS_HZ)	/* Min timeout on reliable sockets */
#define	NFS_MAXREXMIT	100		/* Stop counting after this many */
#define	NFS_MAXWINDOW	1024		/* Max number of outstanding requests */
#define	NFS_RETRANS	10		/* Num of retrans for soft mounts */
#define NFS_FISHY	8		/* Host not responding at this count */
#define	NFS_ATTRTIMEO	5		/* Attribute cache timeout in sec */
#define	NFS_WSIZE	8192		/* Def. write data size <= 8192 */
#define	NFS_RSIZE	8192		/* Def. read data size <= 8192 */
#define	NFS_MAXREADDIR	NFS_MAXDATA	/* Max. size of directory read */
#define	NFS_MAXASYNCDAEMON 20	/* Max. number async_daemons runable */
#define	NMOD(a)		((a) % nfs_asyncdaemons)

/*
 * The set of signals the interrupt an I/O in progress for NFSMNT_INT mounts.
 * What should be in this set is open to debate, but I believe that since
 * I/O system calls on ufs are never interrupted by signals the set should
 * be minimal. My reasoning is that many current programs that use signals
 * such as SIGALRM will not expect file I/O system calls to be interrupted
 * by them and break.
 */
#define	NFSINT_SIGMASK	(sigmask(SIGINT)|sigmask(SIGTERM)|sigmask(SIGKILL)| \
			 sigmask(SIGHUP)|sigmask(SIGQUIT))

/*
 * Socket errors ignored for connectionless sockets??
 * For now, ignore them all
 */
#define	NFSIGNORE_SOERROR(s, e) \
		((e) != EINTR && (e) != ERESTART && (e) != EWOULDBLOCK && \
		((s) & PR_CONNREQUIRED) == 0)

/*
 * Nfs outstanding request list element
 */
struct nfsreq {
	struct nfsreq	*r_next;
	struct nfsreq	*r_prev;
	struct mbuf	*r_mreq;
	struct mbuf	*r_mrep;
	struct nfsmount *r_nmp;
	struct vnode	*r_vp;
	u_long		r_xid;
	short		r_flags;	/* flags on request, see below */
	short		r_retry;	/* max retransmission count */
	short		r_rexmit;	/* current retrans count */
	short		r_timer;	/* tick counter on reply */
	short		r_timerinit;	/* reinit tick counter on reply */
	struct proc	*r_procp;	/* Proc that did I/O system call */
};

/* Flag values for r_flags */
#define R_TIMING	0x01		/* timing request (in mntp) */
#define R_SENT		0x02		/* request has been sent */
#define	R_SOFTTERM	0x04		/* soft mnt, too many retries */
#define	R_INTR		0x08		/* intr mnt, signal pending */
#define	R_SOCKERR	0x10		/* Fatal error on socket */
#define	R_TPRINTFMSG	0x20		/* Did a tprintf msg. */
#define	R_MUSTRESEND	0x40		/* Must resend request */

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
	int	biocache_readlinks;
	int	readlink_bios;
	int	biocache_readdirs;
	int	readdir_bios;
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
