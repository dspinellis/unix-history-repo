/*
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)fifo.h	7.3 (Berkeley) %G%
 */

#ifdef FIFO
/*
 * Prototypes for fifo operations on vnodes.
 */
int	fifo_badop(),
	fifo_ebadf();

int	fifo_lookup __P((
		struct vnode *dvp,
		struct vnode **vpp,
		struct componentname *cnp));
#define fifo_create ((int (*) __P(( \
		struct vnode *dvp, \
 		struct vnode **vpp, \
		struct componentname *cnp, \
		struct vattr *vap))) fifo_badop)
#define fifo_mknod ((int (*) __P(( \
		struct vnode *dvp, \
		struct vnode **vpp, \
		struct componentname *cnp, \
		struct vattr *vap))) fifo_badop)
int	fifo_open __P((
		struct vnode *vp,
		int mode,
		struct ucred *cred,
		struct proc *p));
int	fifo_close __P((
		struct vnode *vp,
		int fflag,
		struct ucred *cred,
		struct proc *p));
#define fifo_access ((int (*) __P(( \
		struct vnode *vp, \
		int mode, \
		struct ucred *cred, \
		struct proc *p))) fifo_ebadf)
#define fifo_getattr ((int (*) __P(( \
		struct vnode *vp, \
		struct vattr *vap, \
		struct ucred *cred, \
		struct proc *p))) fifo_ebadf)
#define fifo_setattr ((int (*) __P(( \
		struct vnode *vp, \
		struct vattr *vap, \
		struct ucred *cred, \
		struct proc *p))) fifo_ebadf)
int	fifo_read __P((
		struct vnode *vp,
		struct uio *uio,
		int ioflag,
		struct ucred *cred));
int	fifo_write __P((
		struct vnode *vp,
		struct uio *uio,
		int ioflag,
		struct ucred *cred));
int	fifo_ioctl __P((
		struct vnode *vp,
		int command,
		caddr_t data,
		int fflag,
		struct ucred *cred,
		struct proc *p));
int	fifo_select __P((
		struct vnode *vp,
		int which,
		int fflags,
		struct ucred *cred,
		struct proc *p));
#define fifo_mmap ((int (*) __P(( \
		struct vnode *vp, \
		int fflags, \
		struct ucred *cred, \
		struct proc *p))) fifo_badop)
#define fifo_fsync ((int (*) __P(( \
		struct vnode *vp, \
		int fflags, \
		struct ucred *cred, \
		int waitfor, \
		struct proc *p))) nullop)
#define fifo_seek ((int (*) __P(( \
		struct vnode *vp, \
		off_t oldoff, \
		off_t newoff, \
		struct ucred *cred))) fifo_badop)
#define fifo_remove ((int (*) __P(( \
		struct vnode *dvp, \
	        struct vnode *vp, \
		struct componentname *cnp))) fifo_badop)
#define fifo_link ((int (*) __P(( \
		register struct vnode *vp, \
		struct vnode *tdvp, \
		struct componentname *cnp))) fifo_badop)
#define fifo_rename ((int (*) __P(( \
		struct vnode *fdvp, \
	        struct vnode *fvp, \
		struct componentname *fcnp, \
		struct vnode *tdvp, \
		struct vnode *tvp, \
		struct componentname *tcnp))) fifo_badop)
#define fifo_mkdir ((int (*) __P(( \
		struct vnode *dvp, \
		struct vnode **vpp, \
		struct componentname *cnp, \
		struct vattr *vap))) fifo_badop)
#define fifo_rmdir ((int (*) __P(( \
		struct vnode *dvp, \
		struct vnode *vp, \
		struct componentname *cnp))) fifo_badop)
#define fifo_symlink ((int (*) __P(( \
		struct vnode *dvp, \
		struct vnode **vpp, \
		struct componentname *cnp, \
		struct vattr *vap, \
		char *target))) fifo_badop)
#define fifo_readdir ((int (*) __P(( \
		struct vnode *vp, \
		struct uio *uio, \
		struct ucred *cred, \
		int *eofflagp))) fifo_badop)
#define fifo_readlink ((int (*) __P(( \
		struct vnode *vp, \
		struct uio *uio, \
		struct ucred *cred))) fifo_badop)
#define fifo_abortop ((int (*) __P(( \
		struct vnode *dvp, \
		struct componentname *cnp))) fifo_badop)
#define fifo_inactive ((int (*) __P(( \
		struct vnode *vp, \
		struct proc *p))) nullop)
#define fifo_reclaim ((int (*) __P(( \
		struct vnode *vp))) nullop)
int	fifo_lock __P((
		struct vnode *vp));
int	fifo_unlock __P((
		struct vnode *vp));
int	fifo_bmap __P((
		struct vnode *vp,
		daddr_t bn,
		struct vnode **vpp,
		daddr_t *bnp));
#define fifo_strategy ((int (*) __P(( \
		struct buf *bp))) fifo_badop)
int	fifo_print __P((
		struct vnode *vp));
#define fifo_islocked ((int (*) __P(( \
		struct vnode *vp))) nullop)
int	fifo_advlock __P((
		struct vnode *vp,
		caddr_t id,
		int op,
		struct flock *fl,
		int flags));
#define fifo_blkatoff ((int (*) __P(( \
		struct vnode *vp, \
		off_t offset, \
		char **res, \
		struct buf **bpp))) fifo_badop)
#define fifo_vget ((int (*) __P(( \
		struct mount *mp, \
		ino_t ino, \
		struct vnode **vpp))) fifo_badop)
#define fifo_valloc ((int (*) __P(( \
		struct vnode *pvp, \
		int mode, \
		struct ucred *cred, \
		struct vnode **vpp))) fifo_badop)
#define fifo_vfree ((void (*) __P(( \
		struct vnode *pvp, \
		ino_t ino, \
		int mode))) fifo_badop)
#define fifo_truncate ((int (*) __P(( \
		struct vnode *vp, \
		u_long length, \
		int flags))) nullop)
#define fifo_update ((int (*) __P(( \
		struct vnode *vp, \
		struct timeval *ta, \
		struct timeval *tm, \
		int waitfor))) nullop)
#define fifo_bwrite ((int (*) __P(( \
		struct buf *bp))) nullop)
#endif /* FIFO */
