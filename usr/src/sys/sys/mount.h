/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)mount.h	7.7 (Berkeley) %G%
 */

typedef quad fsid_t;			/* file system id type */

/*
 * File identifier.
 * These are unique per filesystem on a single machine.
 */
#define	MAXFIDSZ	16

struct fid {
	u_short		fid_len;		/* length of data in bytes */
	char		fid_data[MAXFIDSZ];	/* data (variable length) */
};

/*
 * Structure per mounted file system.
 * Each mounted file system has an array of
 * operations and an instance record.
 * The file systems are put on a doubly linked list.
 */
struct mount {
	struct mount	*m_next;		/* next in mount list */
	struct mount	*m_prev;		/* prev in mount list */
	struct vfsops	*m_op;			/* operations on fs */
	struct vnode	*m_vnodecovered;	/* vnode we mounted on */
	struct vnode	*m_mounth;		/* list of vnodes this mount */
	int		m_flag;			/* flags */
	long		m_fsize;		/* fundamental block size */
	long		m_bsize;		/* optimal transfer size */
	fsid_t		m_fsid;			/* identifier */
	uid_t		m_exroot;		/* exported mapping for uid 0 */
	qaddr_t		m_data;			/* private data */
};

/*
 * Mount flags.
 */
#define	M_RDONLY	0x01		/* read only filesystem */
#define	M_SYNCHRONOUS	0x02		/* file system written synchronously */
#define	M_NOEXEC	0x04		/* can't exec from filesystem */
#define	M_NOSUID	0x08		/* don't honor setuid bits on fs */
#define	M_NODEV		0x10		/* don't interpret special files */
/*
 * exported mount flags.
 */
#define	M_EXPORTED	0x100		/* file system is exported */
#define	M_EXRDONLY	0x200		/* exported read only */
/*
 * filesystem control flags.
 *
 * M_MLOCK lock the mount entry so that name lookup cannot proceed
 * past the mount point.  This keeps the subtree stable during mounts
 * and unmounts.
 */
#define	M_MLOCK		0x1000		/* lock so that subtree is stable */
#define	M_MWAIT		0x2000		/* someone is waiting for lock */
#define	M_UPDATE	0x4000		/* not a real mount, just an update */

/*
 * Operations supported on mounted file system.
 */
struct vfsops {
	int	(*vfs_mount)(	/* mp, path, data, ndp */ );
	int	(*vfs_start)(	/* mp, flags */ );
	int	(*vfs_unmount)(	/* mp, forcibly */ );
	int	(*vfs_root)(	/* mp, vpp */ );
	int	(*vfs_statfs)(	/* mp, sbp */ );
	int	(*vfs_sync)(	/* mp, waitfor */ );
	int	(*vfs_fhtovp)(	/* mp, fidp, vpp */ );
	int	(*vfs_vptofh)(	/* vp, fidp */ );
};

#define VFS_MOUNT(MP, PATH, DATA, NDP) \
	(*(MP)->m_op->vfs_mount)(MP, PATH, DATA, NDP)
#define VFS_START(MP, FLAGS)	  (*(MP)->m_op->vfs_start)(MP, FLAGS)
#define VFS_UNMOUNT(MP, FORCIBLY) (*(MP)->m_op->vfs_unmount)(MP, FORCIBLY)
#define VFS_ROOT(MP, VPP)	  (*(MP)->m_op->vfs_root)(MP, VPP)
#define VFS_STATFS(MP, SBP)	  (*(MP)->m_op->vfs_statfs)(MP, SBP)
#define VFS_SYNC(MP, WAITFOR)	  (*(MP)->m_op->vfs_sync)(MP, WAITFOR)
#define VFS_FHTOVP(MP, FIDP, VPP) (*(MP)->m_op->vfs_fhtovp)(MP, FIDP, VPP)
#define	VFS_VPTOFH(VP, FIDP)	  (*(VP)->v_mount->m_op->vfs_vptofh)(VP, FIDP)

/*
 * forcibly flags for vfs_umount().
 * waitfor flags to vfs_sync()
 */
#define MNT_FORCE	1
#define MNT_NOFORCE	2
#define MNT_WAIT	1
#define MNT_NOWAIT	2

/*
 * file system statistics
 */

#define MNAMELEN 90	/* length of buffer for returned name */

struct statfs {
	short	f_type;			/* type of filesystem (see below) */
	short	f_flags;		/* copy of mount flags */
	long	f_fsize;		/* fundamental file system block size */
	long	f_bsize;		/* optimal transfer block size */
	long	f_blocks;		/* total data blocks in file system */
	long	f_bfree;		/* free blocks in fs */
	long	f_bavail;		/* free blocks avail to non-superuser */
	long	f_files;		/* total file nodes in file system */
	long	f_ffree;		/* free file nodes in fs */
	fsid_t	f_fsid;			/* file system id */
	long	f_spare[9];		/* spare for later */
	char	f_mntonname[MNAMELEN];	/* directory on which mounted */
	char	f_mntfromname[MNAMELEN];/* mounted filesystem */
};
/*
 * File system types.
 */
#define	MOUNT_NONE	0
#define	MOUNT_UFS	1
#define	MOUNT_NFS	2
#define MOUNT_MFS	3
#define	MOUNT_PC	4
#define	MOUNT_MAXTYPE	4

/*
 * Generic file handle
 */
struct fhandle {
	fsid_t	fh_fsid;	/* File system id of mount point */
	struct	fid fh_fid;	/* Id of file */
};
typedef struct fhandle	fhandle_t;

/*
 * Arguments to mount UFS
 */
struct ufs_args {
	char	*fspec;		/* block special device to mount */
	int	exflags;	/* export related flags */
	uid_t	exroot;		/* mapping for root uid */
};

#ifdef MFS
/*
 * Arguments to mount MFS
 */
struct mfs_args {
	char	*name;		/* name to export for statfs */
	caddr_t	base;		/* base address of file system in memory */
	u_long size;		/* size of file system */
};
#endif MFS

#ifdef NFS
/*
 * File Handle (32 bytes for version 2), variable up to 1024 for version 3
 */
union nfsv2fh {
	fhandle_t	fh_generic;
	u_char		fh_bytes[32];
};
typedef union nfsv2fh nfsv2fh_t;

/*
 * Arguments to mount NFS
 */
struct nfs_args {
	struct sockaddr_in	*addr;		/* file server address */
	nfsv2fh_t		*fh;		/* File handle to be mounted */
	int			flags;		/* flags */
	int			wsize;		/* write size in bytes */
	int			rsize;		/* read size in bytes */
	int			timeo;		/* initial timeout in .1 secs */
	int			retrans;	/* times to retry send */
	char			*hostname;	/* server's name */
};
/*
 * NFS mount option flags
 */
#define	NFSMNT_SOFT	0x001	/* soft mount (hard is default) */
#define	NFSMNT_WSIZE	0x002	/* set write size */
#define	NFSMNT_RSIZE	0x004	/* set read size */
#define	NFSMNT_TIMEO	0x008	/* set initial timeout */
#define	NFSMNT_RETRANS	0x010	/* set number of request retrys */
#define	NFSMNT_HOSTNAME	0x020	/* set hostname for error printf */
#define	NFSMNT_INT	0x040	/* allow interrupts on hard mount */
#endif NFS

#ifdef KERNEL
/*
 * exported vnode operations
 */
extern void	vfs_remove();		/* remove a vfs from mounted vfs list */
extern int	vfs_lock();		/* lock a vfs */
extern void	vfs_unlock();		/* unlock a vfs */
extern struct	mount *getvfs();	/* return vfs given fsid */
extern struct	mount *rootfs;		/* ptr to root mount structure */
extern struct	vfsops *vfssw[];	/* mount filesystem type switch table */
#endif
