/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_extern.h	5.2 (Berkeley) %G%
 */

struct fid;
struct inode;
struct mount;
struct nameidata;
struct statfs;
struct proc;
struct ucred;
struct vnode;

daddr_t	 itod __P((LFS *, ino_t));
int	 lfs_balloc __P((LFS *, VNODE *, daddr_t, int, BUF **));
int	 lfs_bmap __P((struct inode *, daddr_t, daddr_t *));
void	 lfs_bwrite __P((struct buf *));
void	 lfs_cleaner __P((void));
int	 lfs_fhtovp __P((struct mount *, struct fid *, struct vnode **));
u_long	 lfs_getversion __P((LFS *fs, ino_t));
ino_t	 lfs_ialloc __P((LFS *, struct inode *, struct inode **,
	     struct ucred *));
IFILE	*lfs_ientry __P((LFS *, ino_t));
struct dinode *
	 lfs_ifind __P((LFS *, ino_t, void *));
void	 lfs_ifree __P((struct inode *));
int	 lfs_inactive __P((struct vnode *, struct proc *));
int	 lfs_init __P((void));
void	 lfs_iset __P((INODE *, daddr_t, time_t));
int	 lfs_lookup __P((struct vnode *, struct nameidata *, struct proc *));
int	 lfs_mount __P((struct mount *, char *, caddr_t, struct nameidata *, struct proc *));
int	 lfs_root __P((struct mount *, struct vnode **));
int	 lfs_segwrite __P((MOUNT *, int));
int	 lfs_statfs __P((struct mount *, struct statfs *, struct proc *));
int	 lfs_sync __P((struct mount *, int));
int	 lfs_unmount __P((struct mount *, int, struct proc *));
int	 lfs_vcreate __P((struct mount *, ino_t, struct vnode **));

#ifdef DEBUG
void	dump_super __P((LFS *));
void	dump_dinode __P((struct dinode *));
void	lfs_print_inumber __P((struct vnode *));
void	lfs_spin __P((void));
#endif

extern struct vnodeops lfs_vnodeops;
