/*
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)fifo.h	7.1 (Berkeley) %G%
 */

#ifdef FIFO
/*
 * Prototypes for fifo operations on vnodes.
 */
int	fifo_badop(),
	fifo_ebadf();

int	fifo_lookup __P((
		struct vnode *vp,
		struct nameidata *ndp,
		struct proc *p));
#define fifo_create ((int (*) __P(( \
		struct nameidata *ndp, \
		struct vattr *vap, \
		struct proc *p))) fifo_badop)
#define fifo_mknod ((int (*) __P(( \
		struct nameidata *ndp, \
		struct vattr *vap, \
		struct ucred *cred, \
		struct proc *p))) fifo_badop)
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
		struct nameidata *ndp, \
		struct proc *p))) fifo_badop)
#define fifo_link ((int (*) __P(( \
		struct vnode *vp, \
		struct nameidata *ndp, \
		struct proc *p))) fifo_badop)
#define fifo_rename ((int (*) __P(( \
		struct nameidata *fndp, \
		struct nameidata *tdnp, \
		struct proc *p))) fifo_badop)
#define fifo_mkdir ((int (*) __P(( \
		struct nameidata *ndp, \
		struct vattr *vap, \
		struct proc *p))) fifo_badop)
#define fifo_rmdir ((int (*) __P(( \
		struct nameidata *ndp, \
		struct proc *p))) fifo_badop)
#define fifo_symlink ((int (*) __P(( \
		struct nameidata *ndp, \
		struct vattr *vap, \
		char *target, \
		struct proc *p))) fifo_badop)
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
		struct nameidata *ndp))) fifo_badop)
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
#endif /* FIFO */
