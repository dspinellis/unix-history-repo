/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)vfs_conf.c	7.8 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/mount.h>
#include <sys/vnode.h>

#ifdef FFS
#include <ufs/ffs/ffs_extern.h>

/*
 * This specifies the filesystem used to mount the root.
 * This specification should be done by /etc/config.
 */
int (*mountroot)() = ffs_mountroot;
#endif

/*
 * These define the root filesystem and device.
 */
struct mount *rootfs;
struct vnode *rootdir;

/*
 * Set up the filesystem operations for vnodes.
 * The types are defined in mount.h.
 */
#ifdef FFS
extern	struct vfsops ufs_vfsops;
#define	UFS_VFSOPS	&ufs_vfsops
#else
#define	UFS_VFSOPS	NULL
#endif

#ifdef LFS
extern	struct vfsops lfs_vfsops;
#define	LFS_VFSOPS	&lfs_vfsops
#else
#define	LFS_VFSOPS	NULL
#endif

#ifdef MFS
extern	struct vfsops mfs_vfsops;
#define	MFS_VFSOPS	&mfs_vfsops
#else
#define	MFS_VFSOPS	NULL
#endif

#ifdef NFS
extern	struct vfsops nfs_vfsops;
#define	NFS_VFSOPS	&nfs_vfsops
#else
#define	NFS_VFSOPS	NULL
#endif

struct vfsops *vfssw[] = {
	NULL,			/* 0 = MOUNT_NONE */
	UFS_VFSOPS,		/* 1 = MOUNT_UFS */
	NFS_VFSOPS,		/* 2 = MOUNT_NFS */
	MFS_VFSOPS,		/* 3 = MOUNT_MFS */
	NULL,			/* 4 = MOUNT_PC */
	LFS_VFSOPS,		/* 5 = MOUNT_LFS */
};


/*
 * Vnode_op_descs lists all vnode operations supported.
 * At boot time vfs_op_init examines this list
 * to configure vnode operations vectors.
 */

extern struct vnodeop_desc
	vop_lookup_desc,
	vop_create_desc,
	vop_mknod_desc,
	vop_open_desc,
	vop_close_desc,
	vop_access_desc,
	vop_getattr_desc,
	vop_setattr_desc,
	vop_read_desc,
	vop_write_desc,
	vop_ioctl_desc,
	vop_select_desc,
	vop_mmap_desc,
	vop_fsync_desc,
	vop_seek_desc,
	vop_remove_desc,
	vop_link_desc,
	vop_rename_desc,
	vop_mkdir_desc,
	vop_rmdir_desc,
	vop_symlink_desc,
	vop_readdir_desc,
	vop_readlink_desc,
	vop_abortop_desc,
	vop_inactive_desc,
	vop_reclaim_desc,
	vop_lock_desc,
	vop_unlock_desc,
	vop_bmap_desc,
	vop_strategy_desc,
	vop_print_desc,
	vop_islocked_desc,
	vop_advlock_desc,
	vop_blkatoff_desc,
	vop_vget_desc,
	vop_valloc_desc,
	vop_vfree_desc,
	vop_truncate_desc,
	vop_update_desc,
	vop_bwrite_desc,
/* and the default */
	vop_default_desc;

struct vnodeop_desc *vfs_op_descs[] = {
	&vop_default_desc,   /* must be first */
	&vop_lookup_desc,
	&vop_create_desc,
	&vop_mknod_desc,
	&vop_open_desc,
	&vop_close_desc,
	&vop_access_desc,
	&vop_getattr_desc,
	&vop_setattr_desc,
	&vop_read_desc,
	&vop_write_desc,
	&vop_ioctl_desc,
	&vop_select_desc,
	&vop_mmap_desc,
	&vop_fsync_desc,
	&vop_seek_desc,
	&vop_remove_desc,
	&vop_link_desc,
	&vop_rename_desc,
	&vop_mkdir_desc,
	&vop_rmdir_desc,
	&vop_symlink_desc,
	&vop_readdir_desc,
	&vop_readlink_desc,
	&vop_abortop_desc,
	&vop_inactive_desc,
	&vop_reclaim_desc,
	&vop_lock_desc,
	&vop_unlock_desc,
	&vop_bmap_desc,
	&vop_strategy_desc,
	&vop_print_desc,
	&vop_islocked_desc,
	&vop_advlock_desc,
	&vop_blkatoff_desc,
	&vop_vget_desc,
	&vop_valloc_desc,
	&vop_vfree_desc,
	&vop_truncate_desc,
	&vop_update_desc,
	&vop_bwrite_desc,
	NULL
};




/*
 *
 * vfs_opv_descs enumerates the list of vnode classes,
 * each with it's own vnode operation vector.
 * It is consulted at system
 * boot to build operation vectors.
 * It's also null terminated.
 *
 * Out-of-kernel, someone else (more knowlegable about what file
 * systems live in this address space) must specify this table.
 */

extern struct vnodeopv_desc ffs_vnodeop_opv_desc;
extern struct vnodeopv_desc ffs_specop_opv_desc;
extern struct vnodeopv_desc ffs_fifoop_opv_desc;
extern struct vnodeopv_desc lfs_vnodeop_opv_desc;
extern struct vnodeopv_desc lfs_specop_opv_desc;
extern struct vnodeopv_desc lfs_fifoop_opv_desc;
extern struct vnodeopv_desc mfs_vnodeop_opv_desc;
extern struct vnodeopv_desc dead_vnodeop_opv_desc;
extern struct vnodeopv_desc fifo_vnodeop_opv_desc;
extern struct vnodeopv_desc spec_vnodeop_opv_desc;
extern struct vnodeopv_desc nfsv2_vnodeop_opv_desc;
extern struct vnodeopv_desc spec_nfsv2nodeop_opv_desc;
extern struct vnodeopv_desc fifo_nfsv2nodeop_opv_desc;


struct vnodeopv_desc *vfs_opv_descs[] = {
	&ffs_vnodeop_opv_desc,
	&ffs_specop_opv_desc,
	&ffs_fifoop_opv_desc,
	&lfs_vnodeop_opv_desc,
	&lfs_specop_opv_desc,
	&lfs_fifoop_opv_desc,
	&mfs_vnodeop_opv_desc,
	&dead_vnodeop_opv_desc,
	&fifo_vnodeop_opv_desc,
	&spec_vnodeop_opv_desc,
	&nfsv2_vnodeop_opv_desc,
	&spec_nfsv2nodeop_opv_desc,
	&fifo_nfsv2nodeop_opv_desc,
	NULL
};

