/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ufsmount.h	7.8 (Berkeley) %G%
 */

/*
 * Mount structure.
 * One allocated on every mount.
 * Used to find the super block.
 */
struct	ufsmount {
	struct	mount *um_mountp;	/* vfs structure for this filesystem */
	dev_t	um_dev;			/* device mounted */
	struct	vnode *um_devvp;	/* vnode for block device mounted */
	struct	fs *um_fs;		/* pointer to superblock */
	struct	vnode *um_quotas[MAXQUOTAS]; /* pointer to quota files */
	struct	ucred *um_cred[MAXQUOTAS]; /* cred for access to quota file */
	time_t	um_btime[MAXQUOTAS];	/* block quota time limit */
	time_t	um_itime[MAXQUOTAS];	/* inode quota time limit */
	char	um_qflags[MAXQUOTAS];	/* quota specific flags, see below */
};
/*
 * Flags describing the state of quotas.
 */
#define	QTF_OPENING	0x01		/* Q_QUOTAON in progress */
#define	QTF_CLOSING	0x02		/* Q_QUOTAOFF in progress */

#ifdef KERNEL
/*
 * Convert mount ptr to ufsmount ptr.
 */
#define VFSTOUFS(mp)	((struct ufsmount *)((mp)->mnt_data))
#endif /* KERNEL */

/*
 * Prototypes for UFS mount operations
 */
int	ufs_mount __P((
		struct mount *mp,
		char *path,
		caddr_t data,
		struct nameidata *ndp,
		struct proc *p));
int	ufs_start __P((
		struct mount *mp,
		int flags,
		struct proc *p));
int	ufs_unmount __P((
		struct mount *mp,
		int mntflags,
		struct proc *p));
int	ufs_root __P((
		struct mount *mp,
		struct vnode **vpp));
int	ufs_quotactl __P((
		struct mount *mp,
		int cmds,
		int uid,	/* should be uid_t */
		caddr_t arg,
		struct proc *p));
int	ufs_statfs __P((
		struct mount *mp,
		struct statfs *sbp,
		struct proc *p));
int	ufs_sync __P((
		struct mount *mp,
		int waitfor));
int	ufs_fhtovp __P((
		struct mount *mp,
		struct fid *fhp,
		struct vnode **vpp));
int	ufs_vptofh __P((
		struct vnode *vp,
		struct fid *fhp));
int	ufs_init __P(());
