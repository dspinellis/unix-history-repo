/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mfsnode.h	7.13 (Berkeley) %G%
 */

/*
 * This structure defines the control data for the memory based file system.
 */

struct mfsnode {
	struct	vnode *mfs_vnode;	/* vnode associated with this mfsnode */
	caddr_t	mfs_baseoff;		/* base of file system in memory */
	long	mfs_size;		/* size of memory file system */
	pid_t	mfs_pid;		/* supporting process pid */
	struct	buf *mfs_buflist;	/* list of I/O requests */
	long	mfs_spare[4];
};

/*
 * Convert between mfsnode pointers and vnode pointers
 */
#define VTOMFS(vp)	((struct mfsnode *)(vp)->v_data)
#define MFSTOV(mfsp)	((mfsp)->mfs_vnode)

/* Prototypes for MFS operations on vnodes. */
#define mfs_lookup ((int (*) __P((struct  vop_lookup_args *)))mfs_badop)
#define mfs_create ((int (*) __P((struct  vop_create_args *)))mfs_badop)
#define mfs_mknod ((int (*) __P((struct  vop_mknod_args *)))mfs_badop)
#define mfs_access ((int (*) __P((struct  vop_access_args *)))mfs_badop)
#define mfs_getattr ((int (*) __P((struct  vop_getattr_args *)))mfs_badop)
#define mfs_setattr ((int (*) __P((struct  vop_setattr_args *)))mfs_badop)
#define mfs_read ((int (*) __P((struct  vop_read_args *)))mfs_badop)
#define mfs_write ((int (*) __P((struct  vop_write_args *)))mfs_badop)
#define mfs_select ((int (*) __P((struct  vop_select_args *)))mfs_badop)
#define mfs_mmap ((int (*) __P((struct  vop_mmap_args *)))mfs_badop)
#define mfs_seek ((int (*) __P((struct  vop_seek_args *)))mfs_badop)
#define mfs_remove ((int (*) __P((struct  vop_remove_args *)))mfs_badop)
#define mfs_link ((int (*) __P((struct  vop_link_args *)))mfs_badop)
#define mfs_rename ((int (*) __P((struct  vop_rename_args *)))mfs_badop)
#define mfs_mkdir ((int (*) __P((struct  vop_mkdir_args *)))mfs_badop)
#define mfs_rmdir ((int (*) __P((struct  vop_rmdir_args *)))mfs_badop)
#define mfs_symlink ((int (*) __P((struct  vop_symlink_args *)))mfs_badop)
#define mfs_readdir ((int (*) __P((struct  vop_readdir_args *)))mfs_badop)
#define mfs_readlink ((int (*) __P((struct  vop_readlink_args *)))mfs_badop)
#define mfs_abortop ((int (*) __P((struct  vop_abortop_args *)))mfs_badop)
#define mfs_lock ((int (*) __P((struct  vop_lock_args *)))nullop)
#define mfs_unlock ((int (*) __P((struct  vop_unlock_args *)))nullop)
#define mfs_islocked ((int (*) __P((struct  vop_islocked_args *)))nullop)
#define mfs_advlock ((int (*) __P((struct  vop_advlock_args *)))mfs_badop)
#define mfs_blkatoff ((int (*) __P((struct  vop_blkatoff_args *)))mfs_badop)
#define mfs_valloc ((int (*) __P((struct  vop_valloc_args *)))mfs_badop)
#define mfs_vfree ((int (*) __P((struct  vop_vfree_args *)))mfs_badop)
#define mfs_truncate ((int (*) __P((struct  vop_truncate_args *)))mfs_badop)
#define mfs_update ((int (*) __P((struct  vop_update_args *)))mfs_badop)
#define mfs_bwrite ((int (*) __P((struct  vop_bwrite_args *)))vn_bwrite)
