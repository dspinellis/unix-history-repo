/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ffs_extern.h	7.3 (Berkeley) %G%
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

__BEGIN_DECLS
int	ffs_alloc __P((struct inode *, daddr_t, daddr_t, int, daddr_t *));
int	ffs_balloc __P((struct inode *, daddr_t, int, struct buf **, int));
int	ffs_blkatoff __P((struct vnode *, off_t, char **, struct buf **));
int	ffs_blkfree __P((struct inode *, daddr_t, off_t));
daddr_t	ffs_blkpref __P((struct inode *, daddr_t, int, daddr_t *));
int	ffs_bmap __P((struct vnode *, daddr_t, struct vnode **, daddr_t *));
void	ffs_clrblock __P((struct fs *, u_char *, daddr_t));
int	ffs_fhtovp __P((struct mount *, struct fid *, struct vnode **));
void	ffs_fragacct __P((struct fs *, int, long [], int));
int	ffs_fsync
	    __P((struct vnode *, int, struct ucred *, int, struct proc *));
int	ffs_inactive __P((struct vnode *, struct proc *));
int	ffs_init __P((void));
int	ffs_isblock __P((struct fs *, u_char *, daddr_t));
int	ffs_mount __P((struct mount *,
	    char *, caddr_t, struct nameidata *, struct proc *));
int	ffs_mountfs __P((struct vnode *, struct mount *, struct proc *));
int	ffs_mountroot __P((void));
int	ffs_read __P((struct vnode *, struct uio *, int, struct ucred *));
int	ffs_realloccg
	    __P((struct inode *, off_t, daddr_t, int, int, struct buf **));
int	ffs_reclaim __P((struct vnode *));
int	ffs_root __P((struct mount *, struct vnode **));
void	ffs_setblock __P((struct fs *, u_char *, daddr_t));
int	ffs_statfs __P((struct mount *, struct statfs *, struct proc *));
int	ffs_sync __P((struct mount *, int));
int	ffs_truncate __P((struct vnode *, u_long, int));
int	ffs_unmount __P((struct mount *, int, struct proc *));
int	ffs_update
	    __P((struct vnode *, struct timeval *, struct timeval *, int));
int	ffs_valloc __P((struct vnode *, int, struct ucred *, struct vnode **));
void	ffs_vfree __P((struct vnode *, ino_t, int));
int	ffs_vget __P((struct mount *, ino_t, struct vnode **));
int	ffs_vptofh __P((struct vnode *, struct fid *));
int	ffs_write __P((struct vnode *, struct uio *, int, struct ucred *));

int	bwrite();		/* FFS needs a bwrite routine.  XXX */

#ifdef DIAGNOSTIC
void	ffs_checkoverlap __P((struct buf *, struct inode *));
#endif
__END_DECLS

extern int inside[], around[];
extern u_char *fragtbl[];
extern struct vnodeops ffs_vnodeops, ffs_specops;
#ifdef FIFO
extern struct vnodeops ffs_fifoops;
#define FFS_FIFOOPS &ffs_fifoops
#else
#define FFS_FIFOOPS NULL
#endif
