/*
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)specdev.h	7.3 (Berkeley) %G%
 */

/*
 * This structure defines the information maintained about
 * special devices. It is allocated in checkalias and freed
 * in vgone.
 */
struct specinfo {
	struct	vnode **si_hashchain;
	struct	vnode *si_specnext;
	long	si_flags;
	dev_t	si_rdev;
};
/*
 * Exported shorthand
 */
#define v_rdev v_specinfo->si_rdev
#define v_hashchain v_specinfo->si_hashchain
#define v_specnext v_specinfo->si_specnext
#define v_specflags v_specinfo->si_flags

/*
 * Flags for specinfo
 */
#define	SI_MOUNTEDON	0x0001	/* block special device is mounted on */

/*
 * Special device management
 */
#define	SPECHSZ	64
#if	((SPECHSZ&(SPECHSZ-1)) == 0)
#define	SPECHASH(rdev)	(((rdev>>5)+(rdev))&(SPECHSZ-1))
#else
#define	SPECHASH(rdev)	(((unsigned)((rdev>>5)+(rdev)))%SPECHSZ)
#endif

struct vnode *speclisth[SPECHSZ];

/*
 * Prototypes for special file operations on vnodes.
 */
int	spec_badop(),
	spec_ebadf();

int	spec_lookup __P((
		struct vnode *vp,
		struct nameidata *ndp,
		struct proc *p));
#define spec_create ((int (*) __P(( \
		struct nameidata *ndp, \
		struct vattr *vap, \
		struct proc *p))) spec_badop)
#define spec_mknod ((int (*) __P(( \
		struct nameidata *ndp, \
		struct vattr *vap, \
		struct ucred *cred, \
		struct proc *p))) spec_badop)
int	spec_open __P((
		struct vnode *vp,
		int mode,
		struct ucred *cred,
		struct proc *p));
int	spec_close __P((
		struct vnode *vp,
		int fflag,
		struct ucred *cred,
		struct proc *p));
#define spec_access ((int (*) __P(( \
		struct vnode *vp, \
		int mode, \
		struct ucred *cred, \
		struct proc *p))) spec_ebadf)
#define spec_getattr ((int (*) __P(( \
		struct vnode *vp, \
		struct vattr *vap, \
		struct ucred *cred, \
		struct proc *p))) spec_ebadf)
#define spec_setattr ((int (*) __P(( \
		struct vnode *vp, \
		struct vattr *vap, \
		struct ucred *cred, \
		struct proc *p))) spec_ebadf)
int	spec_read __P((
		struct vnode *vp,
		struct uio *uio,
		int ioflag,
		struct ucred *cred));
int	spec_write __P((
		struct vnode *vp,
		struct uio *uio,
		int ioflag,
		struct ucred *cred));
int	spec_ioctl __P((
		struct vnode *vp,
		int command,
		caddr_t data,
		int fflag,
		struct ucred *cred,
		struct proc *p));
int	spec_select __P((
		struct vnode *vp,
		int which,
		int fflags,
		struct ucred *cred,
		struct proc *p));
#define spec_mmap ((int (*) __P(( \
		struct vnode *vp, \
		int fflags, \
		struct ucred *cred, \
		struct proc *p))) spec_badop)
#define spec_fsync ((int (*) __P(( \
		struct vnode *vp, \
		int fflags, \
		struct ucred *cred, \
		int waitfor, \
		struct proc *p))) nullop)
#define spec_seek ((int (*) __P(( \
		struct vnode *vp, \
		off_t oldoff, \
		off_t newoff, \
		struct ucred *cred))) spec_badop)
#define spec_remove ((int (*) __P(( \
		struct nameidata *ndp, \
		struct proc *p))) spec_badop)
#define spec_link ((int (*) __P(( \
		struct vnode *vp, \
		struct nameidata *ndp, \
		struct proc *p))) spec_badop)
#define spec_rename ((int (*) __P(( \
		struct nameidata *fndp, \
		struct nameidata *tdnp, \
		struct proc *p))) spec_badop)
#define spec_mkdir ((int (*) __P(( \
		struct nameidata *ndp, \
		struct vattr *vap, \
		struct proc *p))) spec_badop)
#define spec_rmdir ((int (*) __P(( \
		struct nameidata *ndp, \
		struct proc *p))) spec_badop)
#define spec_symlink ((int (*) __P(( \
		struct nameidata *ndp, \
		struct vattr *vap, \
		char *target, \
		struct proc *p))) spec_badop)
#define spec_readdir ((int (*) __P(( \
		struct vnode *vp, \
		struct uio *uio, \
		struct ucred *cred, \
		int *eofflagp))) spec_badop)
#define spec_readlink ((int (*) __P(( \
		struct vnode *vp, \
		struct uio *uio, \
		struct ucred *cred))) spec_badop)
#define spec_abortop ((int (*) __P(( \
		struct nameidata *ndp))) spec_badop)
#define spec_inactive ((int (*) __P(( \
		struct vnode *vp, \
		struct proc *p))) nullop)
#define spec_reclaim ((int (*) __P(( \
		struct vnode *vp))) nullop)
int	spec_lock __P((
		struct vnode *vp));
int	spec_unlock __P((
		struct vnode *vp));
int	spec_bmap __P((
		struct vnode *vp,
		daddr_t bn,
		struct vnode **vpp,
		daddr_t *bnp));
int	spec_strategy __P((
		struct buf *bp));
int	spec_print __P((
		struct vnode *vp));
#define spec_islocked ((int (*) __P(( \
		struct vnode *vp))) nullop)
int	spec_advlock __P((
		struct vnode *vp,
		caddr_t id,
		int op,
		struct flock *fl,
		int flags));
