/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ufs_extern.h	7.1 (Berkeley) %G%
 */

struct buf;
struct direct;
struct disklabel;
struct fid;
struct flock;
struct inode;
struct mount;
struct nameidata;
struct proc;
struct ucred;
struct uio;
struct vattr;
struct vnode;

__BEGIN_DECLS
void	 diskerr
	    __P((struct buf *, char *, char *, int, int, struct disklabel *));
void	 disksort __P((struct buf *, struct buf *));
u_int	 dkcksum __P((struct disklabel *));
char	*readdisklabel __P((dev_t, int (*)(), struct disklabel *));
int	 setdisklabel __P((struct disklabel *, struct disklabel *, u_long));
int	 writedisklabel __P((dev_t, int (*)(), struct disklabel *));

int	 ufs_abortop __P((struct nameidata *));
int	 ufs_access __P((struct vnode *, int, struct ucred *, struct proc *));
int	 ufs_advlock __P((struct vnode *, caddr_t, int, struct flock *, int));
int	 ufs_bmap __P((struct vnode *, daddr_t, struct vnode **, daddr_t *));
void	 ufs_bufstats __P((void));
int	 ufs_checkpath __P((struct inode *, struct inode *, struct ucred *));
int	 ufs_chmod __P((struct vnode *, int, struct proc *));
int	 ufs_chown __P((struct vnode *, u_int, u_int, struct proc *));
int	 ufs_close __P((struct vnode *, int, struct ucred *, struct proc *));
int	 ufs_create __P((struct nameidata *, struct vattr *, struct proc *));
void	 ufs_dirbad __P((struct inode *, off_t, char *));
int	 ufs_dirbadentry __P((struct direct *, int));
int	 ufs_dirempty __P((struct inode *, ino_t, struct ucred *));
int	 ufs_direnter __P((struct inode *, struct nameidata *));
int	 ufs_dirremove __P((struct nameidata *));
int	 ufs_dirrewrite
	    __P((struct inode *, struct inode *, struct nameidata *));
int	 ufs_fhtovp __P((struct mount *, struct fid *, struct vnode **));
int	 ufs_getattr __P((struct vnode *,
	    struct vattr *, struct ucred *, struct proc *));
struct inode *
	 ufs_ihashget __P((int, ino_t));
void	 ufs_ihashinit __P((void));
void	 ufs_ihashins __P((struct inode *));
void	 ufs_ilock __P((struct inode *));
int	 ufs_inactive __P((struct vnode *, struct proc *));
int	 ufs_init __P((void));
int	 ufs_ioctl __P((struct vnode *,
	    int, caddr_t, int, struct ucred *, struct proc *));
void	 ufs_iput __P((struct inode *));
int	 ufs_islocked __P((struct vnode *));
void	 ufs_iunlock __P((struct inode *));
int	 ufs_link __P((struct vnode *, struct nameidata *, struct proc *));
int	 ufs_lock __P((struct vnode *));
int	 ufs_lookup __P((struct vnode *, struct nameidata *, struct proc *));
int	 ufs_mkdir __P((struct nameidata *, struct vattr *, struct proc *));
int	 ufs_mknod __P((struct nameidata *,
	    struct vattr *, struct ucred *, struct proc *));
int	 ufs_mmap __P((struct vnode *, int, struct ucred *, struct proc *));
int	 ufs_mountedon __P((struct vnode *));
int	 ufs_open __P((struct vnode *, int, struct ucred *, struct proc *));
int	 ufs_print __P((struct vnode *));
int	 ufs_readdir __P((struct vnode *, struct uio *, struct ucred *, int *));
int	 ufs_readlink __P((struct vnode *, struct uio *, struct ucred *));
int	 ufs_reclaim __P((struct vnode *));
int	 ufs_remove __P((struct nameidata *, struct proc *));
int	 ufs_rename
	    __P((struct nameidata *, struct nameidata *, struct proc *));
int	 ufs_rmdir __P((struct nameidata *, struct proc *));
int	 ufs_root __P((struct mount *, struct vnode **));
int	 ufs_seek __P((struct vnode *, off_t, off_t, struct ucred *));
int	 ufs_select
	    __P((struct vnode *, int, int, struct ucred *, struct proc *));
int	 ufs_setattr __P((struct vnode *,
	    struct vattr *, struct ucred *, struct proc *));
int	 ufs_start __P((struct mount *, int, struct proc *));
int	 ufs_strategy __P((struct buf *));
int	 ufs_symlink
	    __P((struct nameidata *, struct vattr *, char *, struct proc *));
int	 ufs_unlock __P((struct vnode *));
int	 ufs_vinit __P((struct mount *, struct vnode **));
int	 ufs_vptofh __P((struct vnode *, struct fid *));
int	 ufsspec_close
	    __P((struct vnode *, int, struct ucred *, struct proc *));
int	 ufsspec_read __P((struct vnode *, struct uio *, int, struct ucred *));
int	 ufsspec_write __P((struct vnode *, struct uio *, int, struct ucred *));

#ifdef FIFO
int	ufsfifo_read __P((struct vnode *, struct uio *, int, struct ucred *));
int	ufsfifo_write __P((struct vnode *, struct uio *, int, struct ucred *));
int	ufsfifo_close __P((struct vnode *, int, struct ucred *, struct proc *));
#endif
__END_DECLS
