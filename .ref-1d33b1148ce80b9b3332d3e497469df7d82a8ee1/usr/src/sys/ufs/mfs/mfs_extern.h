/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mfs_extern.h	8.4 (Berkeley) %G%
 */

struct buf;
struct mount;
struct nameidata;
struct proc;
struct statfs;
struct ucred;
struct vnode;
struct vfsconf;

__BEGIN_DECLS
int	mfs_badop __P((void));
int	mfs_bmap __P((struct vop_bmap_args *));
int	mfs_close __P((struct vop_close_args *));
void	mfs_doio __P((struct buf *bp, caddr_t base));
int	mfs_inactive __P((struct vop_inactive_args *)); /* XXX */
int	mfs_reclaim __P((struct vop_reclaim_args *));
int	mfs_init __P((struct vfsconf *));
int	mfs_ioctl __P((struct vop_ioctl_args *));
int	mfs_mount __P((struct mount *mp,
	    char *path, caddr_t data, struct nameidata *ndp, struct proc *p));
int	mfs_open __P((struct vop_open_args *));
int	mfs_print __P((struct vop_print_args *)); /* XXX */
#define	mfs_revoke vop_revoke
int	mfs_start __P((struct mount *mp, int flags, struct proc *p));
int	mfs_statfs __P((struct mount *mp, struct statfs *sbp, struct proc *p));
int	mfs_strategy __P((struct vop_strategy_args *)); /* XXX */
__END_DECLS
