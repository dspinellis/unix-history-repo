/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ffs_extern.h	8.2 (Berkeley) %G%
 */

struct buf;
struct fid;
struct fs;
struct inode;
struct mount;
struct nameidata;
struct proc;
struct statfs;
struct timeval;
struct ucred;
struct uio;
struct vnode;
struct mbuf;

__BEGIN_DECLS
int	ffs_alloc __P((struct inode *,
	    daddr_t, daddr_t, int, struct ucred *, daddr_t *));
int	ffs_balloc __P((struct inode *,
	    daddr_t, int, struct ucred *, struct buf **, int));
int	ffs_blkatoff __P((struct vop_blkatoff_args *));
int	ffs_blkfree __P((struct inode *, daddr_t, long));
daddr_t	ffs_blkpref __P((struct inode *, daddr_t, int, daddr_t *));
int	ffs_bmap __P((struct vop_bmap_args *));
void	ffs_clrblock __P((struct fs *, u_char *, daddr_t));
int	ffs_fhtovp __P((struct mount *, struct fid *, struct mbuf *,
	    struct vnode **, int *, struct ucred **));
void	ffs_fragacct __P((struct fs *, int, long [], int));
int	ffs_fsync __P((struct vop_fsync_args *));
int	ffs_init __P((void));
int	ffs_isblock __P((struct fs *, u_char *, daddr_t));
int	ffs_mount __P((struct mount *,
	    char *, caddr_t, struct nameidata *, struct proc *));
int	ffs_mountfs __P((struct vnode *, struct mount *, struct proc *));
int	ffs_mountroot __P((void));
int	ffs_read __P((struct vop_read_args *));
int	ffs_reallocblks __P((struct vop_reallocblks_args *));
int	ffs_realloccg __P((struct inode *,
	    daddr_t, daddr_t, int, int, struct ucred *, struct buf **));
int	ffs_reclaim __P((struct vop_reclaim_args *));
int	ffs_root __P((struct mount *, struct vnode **));
void	ffs_setblock __P((struct fs *, u_char *, daddr_t));
int	ffs_statfs __P((struct mount *, struct statfs *, struct proc *));
int	ffs_sync __P((struct mount *, int, struct ucred *, struct proc *));
int	ffs_truncate __P((struct vop_truncate_args *));
int	ffs_unmount __P((struct mount *, int, struct proc *));
int	ffs_update __P((struct vop_update_args *));
int	ffs_valloc __P((struct vop_valloc_args *));
int	ffs_vfree __P((struct vop_vfree_args *));
int	ffs_vget __P((struct mount *, ino_t, struct vnode **));
int	ffs_vptofh __P((struct vnode *, struct fid *));
int	ffs_write __P((struct vop_write_args *));

int	bwrite();		/* FFS needs a bwrite routine.  XXX */

#ifdef DIAGNOSTIC
void	ffs_checkoverlap __P((struct buf *, struct inode *));
#endif
__END_DECLS

extern int (**ffs_vnodeop_p)();
extern int (**ffs_specop_p)();
#ifdef FIFO
extern int (**ffs_fifoop_p)();
#define FFS_FIFOOPS ffs_fifoop_p
#else
#define FFS_FIFOOPS NULL
#endif
