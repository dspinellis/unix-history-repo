/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mfs_extern.h	7.1 (Berkeley) %G%
 */

struct buf;
struct mount;
struct nameidata;
struct proc;
struct statfs;
struct ucred;
struct vnode;

__BEGIN_DECLS
int	mfs_badop __P((void));
int	mfs_bmap __P((struct vnode *vp,
	    daddr_t bn, struct vnode **vpp, daddr_t *bnp));
int	mfs_close __P((struct vnode *vp,
	    int flag, struct ucred *cred, struct proc *p));
void	mfs_doio __P((struct buf *bp, caddr_t base));
int	mfs_inactive __P((struct vnode *vp, struct proc *p));
int	mfs_init __P((void));
int	mfs_ioctl __P((struct vnode *vp, int com,
	    caddr_t data, int fflag, struct ucred *cred, struct proc *p));
int	mfs_mount __P((struct mount *mp,
	    char *path, caddr_t data, struct nameidata *ndp, struct proc *p));
int	mfs_open __P((struct vnode *vp,
	    int mode, struct ucred *cred, struct proc *p));
int	mfs_print __P((struct vnode *vp));
int	mfs_start __P((struct mount *mp, int flags, struct proc *p));
int	mfs_statfs __P((struct mount *mp, struct statfs *sbp, struct proc *p));
int	mfs_strategy __P((struct buf *bp));
__END_DECLS
