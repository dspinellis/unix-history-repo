/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_extern.h	7.15 (Berkeley) %G%
 */

struct fid;
struct mount;
struct nameidata;
struct proc;
struct statfs;
struct timeval;
struct inode;
struct uio;

__BEGIN_DECLS
u_long	 cksum __P((void *, size_t));				/* XXX */
int	 lfs_balloc __P((struct vnode *, u_long, daddr_t, struct buf **));
int	 lfs_blkatoff __P((struct vop_blkatoff_args *));
int	 lfs_bmap __P((struct vop_bmap_args *));
int	 lfs_bmaparray
	    __P((struct vnode *, daddr_t, daddr_t *, INDIR *, int *));
int	 lfs_bwrite __P((struct vop_bwrite_args *));
int	 lfs_fhtovp __P((struct mount *, struct fid *, int, struct vnode **));
int	 lfs_fsync __P((struct vop_fsync_args *));
int	 lfs_inactive __P((struct vop_inactive_args *));
int	 lfs_init __P((void));
int	 lfs_makeinode __P((int, struct nameidata *, struct inode **));
int	 lfs_mount __P((struct mount *,
	    char *, caddr_t, struct nameidata *, struct proc *));
int	 lfs_mountroot __P((void));
int	 lfs_read __P((struct vop_read_args *));
int	 lfs_root __P((struct mount *, struct vnode **));
int	 lfs_segwrite __P((struct mount *, int));
int	 lfs_statfs __P((struct mount *, struct statfs *, struct proc *));
int	 lfs_sync __P((struct mount *, int));
int	 lfs_truncate __P((struct vop_truncate_args *));
int	 lfs_unmount __P((struct mount *, int, struct proc *));
int	 lfs_update __P((struct vop_update_args *));
int	 lfs_valloc __P((struct vop_valloc_args *));
int	 lfs_vcreate __P((struct mount *, ino_t, struct vnode **));
void	 lfs_vfree __P((struct vop_vfree_args *));
int	 lfs_vflush __P((struct vnode *));
int	 lfs_vget __P((struct vop_vget_args *));
int	 lfs_vptofh __P((struct vnode *, struct fid *));
int	 lfs_write __P((struct vop_write_args *));
#ifdef DEBUG
void	lfs_dump_dinode __P((struct dinode *));
void	lfs_dump_super __P((struct lfs *));
int	lfs_umountdebug __P((struct mount *));
int	lfs_vinvalbuf __P((struct vnode *));
#endif
__END_DECLS
extern struct vnodeops lfs_vnodeops, lfs_specops;
#ifdef FIFO
extern int (**lfs_fifoop_p)();
#define LFS_FIFOOPS lfs_fifoop_p
#else
#define LFS_FIFOOPS NULL
#endif
