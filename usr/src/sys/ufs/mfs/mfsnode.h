/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mfsnode.h	7.7 (Berkeley) %G%
 */

/*
 * This structure defines the control data for the memory based file system.
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

/* Prototypes for MFS operations on vnodes. */
#define mfs_lookup ((int (*) __P(( \
		struct vnode *dvp, \
		struct vnode **vpp, \
		struct componentname *cnp))) mfs_badop)
#define mfs_create ((int (*) __P(( \
		struct vnode *dvp, \
 		struct vnode **vpp, \
		struct componentname *cnp, \
		struct vattr *vap))) mfs_badop)
#define mfs_mknod ((int (*) __P(( \
		struct vnode *dvp, \
		struct vnode **vpp, \
		struct componentname *cnp, \
		struct vattr *vap))) mfs_badop)
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
		struct vnode *dvp, \
	        struct vnode *vp, \
		struct componentname *cnp))) mfs_badop)
#define mfs_link ((int (*) __P(( \
		register struct vnode *vp, \
		struct vnode *tdvp, \
		struct componentname *cnp))) mfs_badop)
#define mfs_rename ((int (*) __P(( \
		struct vnode *fdvp, \
	        struct vnode *fvp, \
		struct componentname *fcnp, \
		struct vnode *tdvp, \
		struct vnode *tvp, \
		struct componentname *tcnp))) mfs_badop)
#define mfs_mkdir ((int (*) __P(( \
		struct vnode *dvp, \
		struct vnode **vpp, \
		struct componentname *cnp, \
		struct vattr *vap))) mfs_badop)
#define mfs_rmdir ((int (*) __P(( \
		struct vnode *dvp, \
		struct vnode *vp, \
		struct componentname *cnp))) mfs_badop)
#define mfs_symlink ((int (*) __P(( \
		struct vnode *dvp, \
		struct vnode **vpp, \
		struct componentname *cnp, \
		struct vattr *vap, \
		char *target))) mfs_badop)
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
		struct vnode *dvp, \
		struct componentname *cnp))) mfs_badop)
#define mfs_lock ((int (*) __P(( \
		struct vnode *vp))) nullop)
#define mfs_unlock ((int (*) __P(( \
		struct vnode *vp))) nullop)
#define mfs_islocked ((int (*) __P(( \
		struct vnode *vp))) nullop)
#define mfs_advlock ((int (*) __P(( \
		struct vnode *vp, \
		caddr_t id, \
		int op, \
		struct flock *fl, \
		int flags))) mfs_badop)
#define mfs_blkatoff ((int (*) __P(( \
		struct vnode *vp, \
		off_t offset, \
		char **res, \
		struct buf **bpp))) mfs_badop)
#define mfs_vget ((int (*) __P(( \
		struct mount *mp, \
		ino_t ino, \
		struct vnode **vpp))) mfs_badop)
#define mfs_valloc ((int (*) __P(( \
		struct vnode *pvp, \
		int mode, \
		struct ucred *cred, \
		struct vnode **vpp))) mfs_badop)
#define mfs_vfree ((void (*) __P(( \
		struct vnode *pvp, \
		ino_t ino, \
		int mode))) mfs_badop)
#define mfs_truncate ((int (*) __P(( \
		struct vnode *vp, \
		u_long length, \
		int flags))) mfs_badop)
#define mfs_update ((int (*) __P(( \
		struct vnode *vp, \
		struct timeval *ta, \
		struct timeval *tm, \
		int waitfor))) nullop)
#define mfs_bwrite ((int (*) __P(( \
		struct buf *bp))) nullop)
