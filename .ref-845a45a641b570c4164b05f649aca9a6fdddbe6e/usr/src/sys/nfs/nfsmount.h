/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)nfsmount.h	7.11 (Berkeley) %G%
 */

/*
 * Mount structure.
 * One allocated on every NFS mount.
 * Holds NFS specific information for mount.
 */
struct	nfsmount {
	int	nm_flag;		/* Flags for soft/hard... */
	struct	mount *nm_mountp;	/* Vfs structure for this filesystem */
	int	nm_numgrps;		/* Max. size of groupslist */
	nfsv2fh_t nm_fh;		/* File handle of root dir */
	struct	socket *nm_so;		/* Rpc socket */
	int	nm_sotype;		/* Type of socket */
	int	nm_soproto;		/* and protocol */
	int	nm_soflags;		/* pr_flags for socket protocol */
	struct	mbuf *nm_nam;		/* Addr of server */
	int	nm_timeo;		/* Init timer for NFSMNT_DUMBTIMR */
	int	nm_retry;		/* Max retries */
	int	nm_srtt[4];		/* Timers for rpcs */
	int	nm_sdrtt[4];
	int	nm_sent;		/* Request send count */
	int	nm_cwnd;		/* Request send window */
	int	nm_timeouts;		/* Request timeouts */
	int	nm_deadthresh;		/* Threshold of timeouts-->dead server*/
	int	nm_rsize;		/* Max size of read rpc */
	int	nm_wsize;		/* Max size of write rpc */
	int	nm_readahead;		/* Num. of blocks to readahead */
	int	nm_leaseterm;		/* Term (sec) for NQNFS lease */
	struct nfsnode *nm_tnext;	/* Head of lease timer queue */
	struct nfsnode *nm_tprev;
	struct vnode *nm_inprog;	/* Vnode in prog by nqnfs_clientd() */
	uid_t	nm_authuid;		/* Uid for authenticator */
	int	nm_authtype;		/* Authenticator type */
	int	nm_authlen;		/* and length */
	char	*nm_authstr;		/* Authenticator string */
};

#ifdef KERNEL
/*
 * Convert mount ptr to nfsmount ptr.
 */
#define VFSTONFS(mp)	((struct nfsmount *)((mp)->mnt_data))
#endif /* KERNEL */

/*
 * Prototypes for NFS mount operations
 */
int	nfs_mount __P((
		struct mount *mp,
		char *path,
		caddr_t data,
		struct nameidata *ndp,
		struct proc *p));
int	nfs_start __P((
		struct mount *mp,
		int flags,
		struct proc *p));
int	nfs_unmount __P((
		struct mount *mp,
		int mntflags,
		struct proc *p));
int	nfs_root __P((
		struct mount *mp,
		struct vnode **vpp));
int	nfs_quotactl __P((
		struct mount *mp,
		int cmds,
		uid_t uid,
		caddr_t arg,
		struct proc *p));
int	nfs_statfs __P((
		struct mount *mp,
		struct statfs *sbp,
		struct proc *p));
int	nfs_sync __P((
		struct mount *mp,
		int waitfor,
		struct ucred *cred,
		struct proc *p));
int	nfs_fhtovp __P((
		struct mount *mp,
		struct fid *fhp,
		struct mbuf *nam,
		struct vnode **vpp,
		int *exflagsp,
		struct ucred **credanonp));
int	nfs_vptofh __P((
		struct vnode *vp,
		struct fid *fhp));
int	nfs_init __P(());
