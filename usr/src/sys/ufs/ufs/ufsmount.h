/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ufsmount.h	7.10 (Berkeley) %G%
 */

/*
 * The root inode is the root of the file system.  Inode 0 can't be used for
 * normal purposes and historically bad blocks were linked to inode 1, thus
 * the root inode is 2. (inode 1 is no longer used for this purpose, however
 * numerous dump tapes make this assumption, so we are stuck with it).
 */
#define	ROOTINO	((ino_t)2)

struct buf;
struct inode;
struct nameidata;
struct timeval;
struct ucred;
struct uio;
struct vnode;

/* This structure describes the UFS specific mount structure data. */
struct ufsmount {
	struct	mount *um_mountp;		/* filesystem vfs structure */
	dev_t	um_dev;				/* device mounted */
	struct	vnode *um_devvp;		/* block device mounted vnode */
	union {					/* pointer to superblock */
		struct	lfs *lfs;		/* LFS */
		struct	fs *fs;			/* FFS */
	} ufsmount_u;
#define	um_fs	ufsmount_u.fs
#define	um_lfs	ufsmount_u.lfs
	struct	vnode *um_quotas[MAXQUOTAS];	/* pointer to quota files */
	struct	ucred *um_cred[MAXQUOTAS];	/* quota file access cred */
	time_t	um_btime[MAXQUOTAS];		/* block quota time limit */
	time_t	um_itime[MAXQUOTAS];		/* inode quota time limit */
	char	um_qflags[MAXQUOTAS];		/* quota specific flags */

	/*
	 * The following is the inode switch.  It is intended to provide
	 * the interface between the Unix File System semantics and the
	 * on-disk allocation, layout and I/O.
	 */
	int	(*um_blkatoff) __P((struct inode *ip,
		    off_t offset, char **res, struct buf **bpp));
	int	(*um_write) __P((struct vnode *vp,
		    struct uio *uio, int ioflag, struct ucred *cred));
	int	(*um_iget) __P((struct inode *pip,
		    ino_t ino, struct inode **ipp));
	int	(*um_ialloc) __P((struct inode *pip,
		    int mode, struct ucred *cred, struct inode **ipp));
	void	(*um_ifree) __P((struct inode *pip, ino_t ino, int mode));
	int	(*um_itrunc) __P((struct inode *oip, u_long length, int flags));
	int	(*um_iupdat) __P((struct inode *ip,
		    struct timeval *ta, struct timeval *tm, int waitfor));
	int	(*um_bwrite)		/* XXX changes */
		    __P((struct buf *bp));
	int	(*um_bmap)		/* XXX changes */
		    __P((struct inode *ip, daddr_t bn, daddr_t *bnp));
};
/*
 * Flags describing the state of quotas.
 */
#define	QTF_OPENING	0x01			/* Q_QUOTAON in progress */
#define	QTF_CLOSING	0x02			/* Q_QUOTAOFF in progress */

/* Convert mount ptr to ufsmount ptr. */
#define VFSTOUFS(mp)	((struct ufsmount *)((mp)->mnt_data))
