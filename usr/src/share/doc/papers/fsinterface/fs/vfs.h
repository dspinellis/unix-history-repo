/*	@(#)vfs.h 2.7 85/10/31 SMI	*/

typedef long fsid_t[2];			/* file system id type */

/*
 * Structure per mounted file system.
 * Each mounted file system has an array of
 * operations and an instance record.
 * The file systems are put on a doubly linked list.
 */
struct vfs /* why not mount? */ {
	struct vfs	*vfs_next;		/* next vfs in vfs list */
**	struct vfs	*vfs_prev;		/* prev vfs in vfs list */
	struct vfsops	*vfs_op;		/* operations on vfs */
	struct vnode	*vfs_vnodecovered;	/* vnode we mounted on */
	int		vfs_flag;		/* flags */
*	int		vfs_bsize;		/* basic block size */
**	int		vfs_tsize;		/* optimal transfer size */
*	uid_t		vfs_exroot;		/* exported fs uid 0 mapping */
	short		vfs_exflags;		/* exported fs flags */
**	fsid_t		vfs_fsid;		/* identifier */
	caddr_t		vfs_data;		/* private data */
};

/*
 * vfs flags.
 * VFS_MLOCK lock the vfs so that name lookup cannot proceed past the vfs.
 * This keeps the subtree stable during mounts and unmounts.
 */
#define	VFS_RDONLY	0x01		/* read only vfs */
#define	VFS_NOEXEC*	0x02		/* can't exec from filesystem */
#define	VFS_MLOCK	0x04		/* lock vfs so that subtree is stable */
#define	VFS_MWAIT	0x08		/* someone is waiting for lock */
#define	VFS_NOSUID	0x10		/* don't honor setuid bits on vfs */
#define	VFS_EXPORTED	0x20		/* file system is exported (NFS) */

/*
 * exported vfs flags.
 */
#define	EX_RDONLY	0x01		/* exported read only */

/*
 * Operations supported on virtual file system.
 */
struct vfsops {
*	int	(*vfs_mount)(		/* vfs, path, data, datalen */ );
*	int	(*vfs_unmount)(		/* vfs, forcibly */ );
**	int	(*vfs_mountroot)();
	int	(*vfs_root)(		/* vfs, vpp */ );
	int	(*vfs_statfs)(		/* vfs, sbp */ );
*	int	(*vfs_sync)(		/* vfs, waitfor */ );
**	struct vnode *	(*vfs_fhtovp)(	/* vfs, fh */
};

/*
 * file system statistics
 */
struct statfs {
*	short	f_type;			/* type of filesystem */
*	short	f_flags;		/* copy of vfs (mount) flags */
	long	f_bsize;		/* fundamental file system block size */
*	long	f_tsize;		/* optimal transfer block size */
	long	f_blocks;		/* total data blocks in file system */
	long	f_bfree;		/* free blocks in fs */
	long	f_bavail;		/* free blocks avail to non-superuser */
	long	f_files;		/* total file nodes in file system */
	long	f_ffree;		/* free file nodes in fs */
	fsid_t	f_fsid;			/* file system id */
	long	f_spare[7];		/* spare for later */
};

#ifdef KERNEL
/*
 * public operations
 */
extern void	vfs_mountroot();	/* mount the root */
extern int	vfs_add();		/* add a new vfs to mounted vfs list */
extern void	vfs_remove();		/* remove a vfs from mounted vfs list */
extern int	vfs_lock();		/* lock a vfs */
extern void	vfs_unlock();		/* unlock a vfs */

#define VFS_INIT(VFSP, OP, DATA)	{ \
	(VFSP)->vfs_next = (struct vfs *)0; \
	(VFSP)->vfs_op = (OP); \
	(VFSP)->vfs_flag = 0; \
	(VFSP)->vfs_exflags = 0; \
	(VFSP)->vfs_data = (DATA); \
}

/*
 * globals
 */
extern struct vfs *rootvfs;		/* ptr to root vfs structure */

#endif
