/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_extern.h	7.1 (Berkeley) %G%
 */

struct fid;
struct nameidata;
struct statfs;
struct proc;

__BEGIN_DECLS
u_long	 cksum __P((void *, size_t));				/* XXX */
int	 lfs_blkatoff __P((struct inode *, off_t, char **, struct buf **));
int	 lfs_bmap __P((INODE *, daddr_t, daddr_t *));
int	 lfs_bwrite __P((BUF *));
int	 lfs_fsync
	     __P((struct vnode *, int, struct ucred *, int, struct proc *));
u_long	 lfs_getversion __P((struct lfs *fs, ino_t));
int	 lfs_ialloc __P((INODE *, int, UCRED *, INODE **));
DINODE	*lfs_ifind __P((struct lfs *, ino_t, void *));
void	 lfs_ifree __P((INODE *, ino_t, int));
int	 lfs_iget __P((INODE *, ino_t, INODE **));
int	 lfs_inactive __P((VNODE *, struct proc *));
int	 lfs_init __P((void));
void	 lfs_iset __P((INODE *, daddr_t, time_t));
daddr_t	 lfs_itod __P((struct lfs *, ino_t));
int	 lfs_itrunc __P((INODE *, u_long, int));
int	 lfs_iupdat
	     __P((struct inode *, struct timeval *, struct timeval *, int));
int	 lfs_makeinode __P((int, struct nameidata *, struct inode **));
int	 lfs_mount
	     __P((MOUNT *, char *, caddr_t, struct nameidata *, struct proc *));
int	 lfs_mountroot __P((void));
int	 lfs_read __P((struct vnode *, struct uio *, int, struct ucred *));
int	 lfs_segwrite __P((MOUNT *, int));
int	 lfs_statfs __P((MOUNT *, struct statfs *, struct proc *));
int	 lfs_sync __P((MOUNT *, int));
int	 lfs_unmount __P((MOUNT *, int, struct proc *));
int	 lfs_vcreate __P((MOUNT *, ino_t, VNODE **));
int	 lfs_write __P((struct vnode *, struct uio *, int, struct ucred *));

#ifdef DEBUG
void	lfs_dump_dinode __P((DINODE *));
void	lfs_dump_super __P((struct lfs *));
int	lfs_umountdebug __P((struct mount *));
int	lfs_vinvalbuf __P((struct vnode *));
#endif
__END_DECLS

extern struct vnodeops lfs_vnodeops;
