/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mfsnode.h	7.3 (Berkeley) %G%
 */

/*
 * This structure defines the control data for the memory
 * based file system.
 */

struct mfsnode {
	struct	vnode *mfs_vnode;	/* vnode associated with this mfsnode */
	caddr_t	mfs_baseoff;		/* base of file system in memory */
	long	mfs_size;		/* size of memory file system */
	pid_t	mfs_pid;		/* supporting process pid */
	struct	buf *mfs_buflist;	/* list of I/O requests */
	long	mfs_spare[4];
};

/*
 * Convert between mfsnode pointers and vnode pointers
 */
#define VTOMFS(vp)	((struct mfsnode *)(vp)->v_data)
#define MFSTOV(mfsp)	((mfsp)->mfs_vnode)

/*
 * Prototypes for MFS operations on vnodes.
 */
int	mfs_badop();
#define mfs_lookup ((int (*) __P(( \
		struct vnode *vp, \
		struct nameidata *ndp, \
		struct proc *p))) mfs_badop)
#define mfs_create ((int (*) __P(( \
		struct nameidata *ndp, \
		struct vattr *vap, \
		struct proc *p))) mfs_badop)
#define mfs_mknod ((int (*) __P(( \
		struct nameidata *ndp, \
		struct vattr *vap, \
		struct ucred *cred, \
		struct proc *p))) mfs_badop)
int	mfs_open __P((
		struct vnode *vp,
		int mode,
		struct ucred *cred,
		struct proc *p));
int	mfs_close __P((
		struct vnode *vp,
		int fflag,
		struct ucred *cred,
		struct proc *p));
#define mfs_access ((int (*) __P(( \
		struct vnode *vp, \
		int mode, \
		struct ucred *cred, \
		struct proc *p))) mfs_badop)
#define mfs_getattr ((int (*) __P(( \
		struct vnode *vp, \
		struct vattr *vap, \
		struct ucred *cred, \
		struct proc *p))) mfs_badop)
#define mfs_setattr ((int (*) __P(( \
		struct vnode *vp, \
		struct vattr *vap, \
		struct ucred *cred, \
		struct proc *p))) mfs_badop)
#define mfs_read ((int (*) __P(( \
		struct vnode *vp, \
		struct uio *uio, \
		int ioflag, \
		struct ucred *cred))) mfs_badop)
#define mfs_write ((int (*) __P(( \
		struct vnode *vp, \
		struct uio *uio, \
		int ioflag, \
		struct ucred *cred))) mfs_badop)
int	mfs_ioctl __P((
		struct vnode *vp,
		int command,
		caddr_t data,
		int fflag,
		struct ucred *cred,
		struct proc *p));
#define mfs_select ((int (*) __P(( \
		struct vnode *vp, \
		int which, \
		int fflags, \
		struct ucred *cred, \
		struct proc *p))) mfs_badop)
#define mfs_mmap ((int (*) __P(( \
		struct vnode *vp, \
		int fflags, \
		struct ucred *cred, \
		struct proc *p))) mfs_badop)
#define mfs_fsync ((int (*) __P(( \
		struct vnode *vp, \
		int fflags, \
		struct ucred *cred, \
		int waitfor, \
		struct proc *p))) mfs_badop)
#define mfs_seek ((int (*) __P(( \
		struct vnode *vp, \
		off_t oldoff, \
		off_t newoff, \
		struct ucred *cred))) mfs_badop)
#define mfs_remove ((int (*) __P(( \
		struct nameidata *ndp, \
		struct proc *p))) mfs_badop)
#define mfs_link ((int (*) __P(( \
		struct vnode *vp, \
		struct nameidata *ndp, \
		struct proc *p))) mfs_badop)
#define mfs_rename ((int (*) __P(( \
		struct nameidata *fndp, \
		struct nameidata *tdnp, \
		struct proc *p))) mfs_badop)
#define mfs_mkdir ((int (*) __P(( \
		struct nameidata *ndp, \
		struct vattr *vap, \
		struct proc *p))) mfs_badop)
#define mfs_rmdir ((int (*) __P(( \
		struct nameidata *ndp, \
		struct proc *p))) mfs_badop)
#define mfs_symlink ((int (*) __P(( \
		struct nameidata *ndp, \
		struct vattr *vap, \
		char *target, \
		struct proc *p))) mfs_badop)
#define mfs_readdir ((int (*) __P(( \
		struct vnode *vp, \
		struct uio *uio, \
		struct ucred *cred, \
		int *eofflagp))) mfs_badop)
#define mfs_readlink ((int (*) __P(( \
		struct vnode *vp, \
		struct uio *uio, \
		struct ucred *cred))) mfs_badop)
#define mfs_abortop ((int (*) __P(( \
		struct nameidata *ndp))) mfs_badop)
int	mfs_inactive __P((
		struct vnode *vp,
		struct proc *p));
#define mfs_reclaim ((int (*) __P(( \
		struct vnode *vp))) nullop)
#define mfs_lock ((int (*) __P(( \
		struct vnode *vp))) nullop)
#define mfs_unlock ((int (*) __P(( \
		struct vnode *vp))) nullop)
int	mfs_bmap __P((
		struct vnode *vp,
		daddr_t bn,
		struct vnode **vpp,
		daddr_t *bnp));
int	mfs_strategy __P((
		struct buf *bp));
int	mfs_print __P((
		struct vnode *vp));
#define mfs_islocked ((int (*) __P(( \
		struct vnode *vp))) nullop)
#define mfs_advlock ((int (*) __P(( \
		struct vnode *vp, \
		caddr_t id, \
		int op, \
		struct flock *fl, \
		int flags))) mfs_badop)
