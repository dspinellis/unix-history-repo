/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)vnode_if.h	7.1 (Berkeley) %G%
 */

/*
 * This file should be automatically generated from a corresponding .int file.
 */

struct vop_lookup_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_dvp;
	struct vnode ** a_vpp;
	struct componentname * a_cnp;
};
extern struct vnodeop_desc vop_lookup_desc;
#define USES_VOP_LOOKUP struct vop_lookup_args vop_lookup_a
#define VOP_LOOKUP(A0,A1,A2) \
		( vop_lookup_a.a_desc = VDESC(vop_lookup), \
		vop_lookup_a.a_dvp=(A0), \
		vop_lookup_a.a_vpp=(A1), \
		vop_lookup_a.a_cnp=(A2), \
		(VCALL((A0),VOFFSET(vop_lookup),&vop_lookup_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_create_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_dvp;
	struct vnode ** a_vpp;
	struct componentname * a_cnp;
	struct vattr * a_vap;
};
extern struct vnodeop_desc vop_create_desc;
#define USES_VOP_CREATE struct vop_create_args vop_create_a
#define VOP_CREATE(A0,A1,A2,A3) \
		( vop_create_a.a_desc = VDESC(vop_create), \
		vop_create_a.a_dvp=(A0), \
		vop_create_a.a_vpp=(A1), \
		vop_create_a.a_cnp=(A2), \
		vop_create_a.a_vap=(A3), \
		(VCALL((A0),VOFFSET(vop_create),&vop_create_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_mknod_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_dvp;
	struct vnode ** a_vpp;
	struct componentname * a_cnp;
	struct vattr * a_vap;
};
extern struct vnodeop_desc vop_mknod_desc;
#define USES_VOP_MKNOD struct vop_mknod_args vop_mknod_a
#define VOP_MKNOD(A0,A1,A2,A3) \
		( vop_mknod_a.a_desc = VDESC(vop_mknod), \
		vop_mknod_a.a_dvp=(A0), \
		vop_mknod_a.a_vpp=(A1), \
		vop_mknod_a.a_cnp=(A2), \
		vop_mknod_a.a_vap=(A3), \
		(VCALL((A0),VOFFSET(vop_mknod),&vop_mknod_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_open_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_vp;
	int  a_mode;
	struct ucred * a_cred;
	struct proc * a_p;
};
extern struct vnodeop_desc vop_open_desc;
#define USES_VOP_OPEN struct vop_open_args vop_open_a
#define VOP_OPEN(A0,A1,A2,A3) \
		( vop_open_a.a_desc = VDESC(vop_open), \
		vop_open_a.a_vp=(A0), \
		vop_open_a.a_mode=(A1), \
		vop_open_a.a_cred=(A2), \
		vop_open_a.a_p=(A3), \
		(VCALL((A0),VOFFSET(vop_open),&vop_open_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_close_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_vp;
	int  a_fflag;
	struct ucred * a_cred;
	struct proc * a_p;
};
extern struct vnodeop_desc vop_close_desc;
#define USES_VOP_CLOSE struct vop_close_args vop_close_a
#define VOP_CLOSE(A0,A1,A2,A3) \
		( vop_close_a.a_desc = VDESC(vop_close), \
		vop_close_a.a_vp=(A0), \
		vop_close_a.a_fflag=(A1), \
		vop_close_a.a_cred=(A2), \
		vop_close_a.a_p=(A3), \
		(VCALL((A0),VOFFSET(vop_close),&vop_close_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_access_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_vp;
	int  a_mode;
	struct ucred * a_cred;
	struct proc * a_p;
};
extern struct vnodeop_desc vop_access_desc;
#define USES_VOP_ACCESS struct vop_access_args vop_access_a
#define VOP_ACCESS(A0,A1,A2,A3) \
		( vop_access_a.a_desc = VDESC(vop_access), \
		vop_access_a.a_vp=(A0), \
		vop_access_a.a_mode=(A1), \
		vop_access_a.a_cred=(A2), \
		vop_access_a.a_p=(A3), \
		(VCALL((A0),VOFFSET(vop_access),&vop_access_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_getattr_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_vp;
	struct vattr * a_vap;
	struct ucred * a_cred;
	struct proc * a_p;
};
extern struct vnodeop_desc vop_getattr_desc;
#define USES_VOP_GETATTR struct vop_getattr_args vop_getattr_a
#define VOP_GETATTR(A0,A1,A2,A3) \
		( vop_getattr_a.a_desc = VDESC(vop_getattr), \
		vop_getattr_a.a_vp=(A0), \
		vop_getattr_a.a_vap=(A1), \
		vop_getattr_a.a_cred=(A2), \
		vop_getattr_a.a_p=(A3), \
		(VCALL((A0),VOFFSET(vop_getattr),&vop_getattr_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_setattr_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_vp;
	struct vattr * a_vap;
	struct ucred * a_cred;
	struct proc * a_p;
};
extern struct vnodeop_desc vop_setattr_desc;
#define USES_VOP_SETATTR struct vop_setattr_args vop_setattr_a
#define VOP_SETATTR(A0,A1,A2,A3) \
		( vop_setattr_a.a_desc = VDESC(vop_setattr), \
		vop_setattr_a.a_vp=(A0), \
		vop_setattr_a.a_vap=(A1), \
		vop_setattr_a.a_cred=(A2), \
		vop_setattr_a.a_p=(A3), \
		(VCALL((A0),VOFFSET(vop_setattr),&vop_setattr_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_read_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_vp;
	struct uio * a_uio;
	int  a_ioflag;
	struct ucred * a_cred;
};
extern struct vnodeop_desc vop_read_desc;
#define USES_VOP_READ struct vop_read_args vop_read_a
#define VOP_READ(A0,A1,A2,A3) \
		( vop_read_a.a_desc = VDESC(vop_read), \
		vop_read_a.a_vp=(A0), \
		vop_read_a.a_uio=(A1), \
		vop_read_a.a_ioflag=(A2), \
		vop_read_a.a_cred=(A3), \
		(VCALL((A0),VOFFSET(vop_read),&vop_read_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_write_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_vp;
	struct uio * a_uio;
	int  a_ioflag;
	struct ucred * a_cred;
};
extern struct vnodeop_desc vop_write_desc;
#define USES_VOP_WRITE struct vop_write_args vop_write_a
#define VOP_WRITE(A0,A1,A2,A3) \
		( vop_write_a.a_desc = VDESC(vop_write), \
		vop_write_a.a_vp=(A0), \
		vop_write_a.a_uio=(A1), \
		vop_write_a.a_ioflag=(A2), \
		vop_write_a.a_cred=(A3), \
		(VCALL((A0),VOFFSET(vop_write),&vop_write_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_ioctl_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_vp;
	int  a_command;
	caddr_t  a_data;
	int  a_fflag;
	struct ucred * a_cred;
	struct proc * a_p;
};
extern struct vnodeop_desc vop_ioctl_desc;
#define USES_VOP_IOCTL struct vop_ioctl_args vop_ioctl_a
#define VOP_IOCTL(A0,A1,A2,A3,A4,A5) \
		( vop_ioctl_a.a_desc = VDESC(vop_ioctl), \
		vop_ioctl_a.a_vp=(A0), \
		vop_ioctl_a.a_command=(A1), \
		vop_ioctl_a.a_data=(A2), \
		vop_ioctl_a.a_fflag=(A3), \
		vop_ioctl_a.a_cred=(A4), \
		vop_ioctl_a.a_p=(A5), \
		(VCALL((A0),VOFFSET(vop_ioctl),&vop_ioctl_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_select_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_vp;
	int  a_which;
	int  a_fflags;
	struct ucred * a_cred;
	struct proc * a_p;
};
extern struct vnodeop_desc vop_select_desc;
#define USES_VOP_SELECT struct vop_select_args vop_select_a
#define VOP_SELECT(A0,A1,A2,A3,A4) \
		( vop_select_a.a_desc = VDESC(vop_select), \
		vop_select_a.a_vp=(A0), \
		vop_select_a.a_which=(A1), \
		vop_select_a.a_fflags=(A2), \
		vop_select_a.a_cred=(A3), \
		vop_select_a.a_p=(A4), \
		(VCALL((A0),VOFFSET(vop_select),&vop_select_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_mmap_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_vp;
	int  a_fflags;
	struct ucred * a_cred;
	struct proc * a_p;
};
extern struct vnodeop_desc vop_mmap_desc;
#define USES_VOP_MMAP struct vop_mmap_args vop_mmap_a
#define VOP_MMAP(A0,A1,A2,A3) \
		( vop_mmap_a.a_desc = VDESC(vop_mmap), \
		vop_mmap_a.a_vp=(A0), \
		vop_mmap_a.a_fflags=(A1), \
		vop_mmap_a.a_cred=(A2), \
		vop_mmap_a.a_p=(A3), \
		(VCALL((A0),VOFFSET(vop_mmap),&vop_mmap_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_fsync_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_vp;
	int  a_fflags;
	struct ucred * a_cred;
	int  a_waitfor;
	struct proc * a_p;
};
extern struct vnodeop_desc vop_fsync_desc;
#define USES_VOP_FSYNC struct vop_fsync_args vop_fsync_a
#define VOP_FSYNC(A0,A1,A2,A3,A4) \
		( vop_fsync_a.a_desc = VDESC(vop_fsync), \
		vop_fsync_a.a_vp=(A0), \
		vop_fsync_a.a_fflags=(A1), \
		vop_fsync_a.a_cred=(A2), \
		vop_fsync_a.a_waitfor=(A3), \
		vop_fsync_a.a_p=(A4), \
		(VCALL((A0),VOFFSET(vop_fsync),&vop_fsync_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_seek_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_vp;
	off_t  a_oldoff;
	off_t  a_newoff;
	struct ucred * a_cred;
};
extern struct vnodeop_desc vop_seek_desc;
#define USES_VOP_SEEK struct vop_seek_args vop_seek_a
#define VOP_SEEK(A0,A1,A2,A3) \
		( vop_seek_a.a_desc = VDESC(vop_seek), \
		vop_seek_a.a_vp=(A0), \
		vop_seek_a.a_oldoff=(A1), \
		vop_seek_a.a_newoff=(A2), \
		vop_seek_a.a_cred=(A3), \
		(VCALL((A0),VOFFSET(vop_seek),&vop_seek_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_remove_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_dvp;
	struct vnode * a_vp;
	struct componentname * a_cnp;
};
extern struct vnodeop_desc vop_remove_desc;
#define USES_VOP_REMOVE struct vop_remove_args vop_remove_a
#define VOP_REMOVE(A0,A1,A2) \
		( vop_remove_a.a_desc = VDESC(vop_remove), \
		vop_remove_a.a_dvp=(A0), \
		vop_remove_a.a_vp=(A1), \
		vop_remove_a.a_cnp=(A2), \
		(VCALL((A0),VOFFSET(vop_remove),&vop_remove_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_link_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_vp;
	struct vnode * a_tdvp;
	struct componentname * a_cnp;
};
extern struct vnodeop_desc vop_link_desc;
#define USES_VOP_LINK struct vop_link_args vop_link_a
#define VOP_LINK(A0,A1,A2) \
		( vop_link_a.a_desc = VDESC(vop_link), \
		vop_link_a.a_vp=(A0), \
		vop_link_a.a_tdvp=(A1), \
		vop_link_a.a_cnp=(A2), \
		(VCALL((A0),VOFFSET(vop_link),&vop_link_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_rename_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_fdvp;
	struct vnode * a_fvp;
	struct componentname * a_fcnp;
	struct vnode * a_tdvp;
	struct vnode * a_tvp;
	struct componentname * a_tcnp;
};
extern struct vnodeop_desc vop_rename_desc;
#define USES_VOP_RENAME struct vop_rename_args vop_rename_a
#define VOP_RENAME(A0,A1,A2,A3,A4,A5) \
		( vop_rename_a.a_desc = VDESC(vop_rename), \
		vop_rename_a.a_fdvp=(A0), \
		vop_rename_a.a_fvp=(A1), \
		vop_rename_a.a_fcnp=(A2), \
		vop_rename_a.a_tdvp=(A3), \
		vop_rename_a.a_tvp=(A4), \
		vop_rename_a.a_tcnp=(A5), \
		(VCALL((A0),VOFFSET(vop_rename),&vop_rename_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_mkdir_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_dvp;
	struct vnode ** a_vpp;
	struct componentname * a_cnp;
	struct vattr * a_vap;
};
extern struct vnodeop_desc vop_mkdir_desc;
#define USES_VOP_MKDIR struct vop_mkdir_args vop_mkdir_a
#define VOP_MKDIR(A0,A1,A2,A3) \
		( vop_mkdir_a.a_desc = VDESC(vop_mkdir), \
		vop_mkdir_a.a_dvp=(A0), \
		vop_mkdir_a.a_vpp=(A1), \
		vop_mkdir_a.a_cnp=(A2), \
		vop_mkdir_a.a_vap=(A3), \
		(VCALL((A0),VOFFSET(vop_mkdir),&vop_mkdir_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_rmdir_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_dvp;
	struct vnode * a_vp;
	struct componentname * a_cnp;
};
extern struct vnodeop_desc vop_rmdir_desc;
#define USES_VOP_RMDIR struct vop_rmdir_args vop_rmdir_a
#define VOP_RMDIR(A0,A1,A2) \
		( vop_rmdir_a.a_desc = VDESC(vop_rmdir), \
		vop_rmdir_a.a_dvp=(A0), \
		vop_rmdir_a.a_vp=(A1), \
		vop_rmdir_a.a_cnp=(A2), \
		(VCALL((A0),VOFFSET(vop_rmdir),&vop_rmdir_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_symlink_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_dvp;
	struct vnode ** a_vpp;
	struct componentname * a_cnp;
	struct vattr * a_vap;
	char * a_target;
};
extern struct vnodeop_desc vop_symlink_desc;
#define USES_VOP_SYMLINK struct vop_symlink_args vop_symlink_a
#define VOP_SYMLINK(A0,A1,A2,A3,A4) \
		( vop_symlink_a.a_desc = VDESC(vop_symlink), \
		vop_symlink_a.a_dvp=(A0), \
		vop_symlink_a.a_vpp=(A1), \
		vop_symlink_a.a_cnp=(A2), \
		vop_symlink_a.a_vap=(A3), \
		vop_symlink_a.a_target=(A4), \
		(VCALL((A0),VOFFSET(vop_symlink),&vop_symlink_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_readdir_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_vp;
	struct uio * a_uio;
	struct ucred * a_cred;
	int * a_eofflagp;
};
extern struct vnodeop_desc vop_readdir_desc;
#define USES_VOP_READDIR struct vop_readdir_args vop_readdir_a
#define VOP_READDIR(A0,A1,A2,A3) \
		( vop_readdir_a.a_desc = VDESC(vop_readdir), \
		vop_readdir_a.a_vp=(A0), \
		vop_readdir_a.a_uio=(A1), \
		vop_readdir_a.a_cred=(A2), \
		vop_readdir_a.a_eofflagp=(A3), \
		(VCALL((A0),VOFFSET(vop_readdir),&vop_readdir_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_readlink_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_vp;
	struct uio * a_uio;
	struct ucred * a_cred;
};
extern struct vnodeop_desc vop_readlink_desc;
#define USES_VOP_READLINK struct vop_readlink_args vop_readlink_a
#define VOP_READLINK(A0,A1,A2) \
		( vop_readlink_a.a_desc = VDESC(vop_readlink), \
		vop_readlink_a.a_vp=(A0), \
		vop_readlink_a.a_uio=(A1), \
		vop_readlink_a.a_cred=(A2), \
		(VCALL((A0),VOFFSET(vop_readlink),&vop_readlink_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_abortop_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_dvp;
	struct componentname * a_cnp;
};
extern struct vnodeop_desc vop_abortop_desc;
#define USES_VOP_ABORTOP struct vop_abortop_args vop_abortop_a
#define VOP_ABORTOP(A0,A1) \
		( vop_abortop_a.a_desc = VDESC(vop_abortop), \
		vop_abortop_a.a_dvp=(A0), \
		vop_abortop_a.a_cnp=(A1), \
		(VCALL((A0),VOFFSET(vop_abortop),&vop_abortop_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_inactive_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_vp;
	struct proc * a_p;
};
extern struct vnodeop_desc vop_inactive_desc;
#define USES_VOP_INACTIVE struct vop_inactive_args vop_inactive_a
#define VOP_INACTIVE(A0,A1) \
		( vop_inactive_a.a_desc = VDESC(vop_inactive), \
		vop_inactive_a.a_vp=(A0), \
		vop_inactive_a.a_p=(A1), \
		(VCALL((A0),VOFFSET(vop_inactive),&vop_inactive_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_reclaim_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_vp;
};
extern struct vnodeop_desc vop_reclaim_desc;
#define USES_VOP_RECLAIM struct vop_reclaim_args vop_reclaim_a
#define VOP_RECLAIM(A0) \
		( vop_reclaim_a.a_desc = VDESC(vop_reclaim), \
		vop_reclaim_a.a_vp=(A0), \
		(VCALL((A0),VOFFSET(vop_reclaim),&vop_reclaim_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_lock_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_vp;
};
extern struct vnodeop_desc vop_lock_desc;
#define USES_VOP_LOCK struct vop_lock_args vop_lock_a
#define VOP_LOCK(A0) \
		( vop_lock_a.a_desc = VDESC(vop_lock), \
		vop_lock_a.a_vp=(A0), \
		(VCALL((A0),VOFFSET(vop_lock),&vop_lock_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_unlock_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_vp;
};
extern struct vnodeop_desc vop_unlock_desc;
#define USES_VOP_UNLOCK struct vop_unlock_args vop_unlock_a
#define VOP_UNLOCK(A0) \
		( vop_unlock_a.a_desc = VDESC(vop_unlock), \
		vop_unlock_a.a_vp=(A0), \
		(VCALL((A0),VOFFSET(vop_unlock),&vop_unlock_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_bmap_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_vp;
	daddr_t  a_bn;
	struct vnode ** a_vpp;
	daddr_t * a_bnp;
};
extern struct vnodeop_desc vop_bmap_desc;
#define USES_VOP_BMAP struct vop_bmap_args vop_bmap_a
#define VOP_BMAP(A0,A1,A2,A3) \
		( vop_bmap_a.a_desc = VDESC(vop_bmap), \
		vop_bmap_a.a_vp=(A0), \
		vop_bmap_a.a_bn=(A1), \
		vop_bmap_a.a_vpp=(A2), \
		vop_bmap_a.a_bnp=(A3), \
		(VCALL((A0),VOFFSET(vop_bmap),&vop_bmap_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_strategy_args {
	struct vnodeop_desc *a_desc;
	struct buf * a_bp;
};
extern struct vnodeop_desc vop_strategy_desc;
#define USES_VOP_STRATEGY struct vop_strategy_args vop_strategy_a
#define VOP_STRATEGY(A0) \
		( vop_strategy_a.a_desc = VDESC(vop_strategy), \
		vop_strategy_a.a_bp=(A0), \
		(VCALL((A0),VOFFSET(vop_strategy),&vop_strategy_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_print_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_vp;
};
extern struct vnodeop_desc vop_print_desc;
#define USES_VOP_PRINT struct vop_print_args vop_print_a
#define VOP_PRINT(A0) \
		( vop_print_a.a_desc = VDESC(vop_print), \
		vop_print_a.a_vp=(A0), \
		(VCALL((A0),VOFFSET(vop_print),&vop_print_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_islocked_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_vp;
};
extern struct vnodeop_desc vop_islocked_desc;
#define USES_VOP_ISLOCKED struct vop_islocked_args vop_islocked_a
#define VOP_ISLOCKED(A0) \
		( vop_islocked_a.a_desc = VDESC(vop_islocked), \
		vop_islocked_a.a_vp=(A0), \
		(VCALL((A0),VOFFSET(vop_islocked),&vop_islocked_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_advlock_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_vp;
	caddr_t  a_id;
	int  a_op;
	struct flock * a_fl;
	int  a_flags;
};
extern struct vnodeop_desc vop_advlock_desc;
#define USES_VOP_ADVLOCK struct vop_advlock_args vop_advlock_a
#define VOP_ADVLOCK(A0,A1,A2,A3,A4) \
		( vop_advlock_a.a_desc = VDESC(vop_advlock), \
		vop_advlock_a.a_vp=(A0), \
		vop_advlock_a.a_id=(A1), \
		vop_advlock_a.a_op=(A2), \
		vop_advlock_a.a_fl=(A3), \
		vop_advlock_a.a_flags=(A4), \
		(VCALL((A0),VOFFSET(vop_advlock),&vop_advlock_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_blkatoff_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_vp;
	off_t  a_offset;
	char ** a_res;
	struct buf ** a_bpp;
};
extern struct vnodeop_desc vop_blkatoff_desc;
#define USES_VOP_BLKATOFF struct vop_blkatoff_args vop_blkatoff_a
#define VOP_BLKATOFF(A0,A1,A2,A3) \
		( vop_blkatoff_a.a_desc = VDESC(vop_blkatoff), \
		vop_blkatoff_a.a_vp=(A0), \
		vop_blkatoff_a.a_offset=(A1), \
		vop_blkatoff_a.a_res=(A2), \
		vop_blkatoff_a.a_bpp=(A3), \
		(VCALL((A0),VOFFSET(vop_blkatoff),&vop_blkatoff_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_vget_args {
	struct vnodeop_desc *a_desc;
	struct mount * a_mp;
	ino_t  a_ino;
	struct vnode ** a_vpp;
};
extern struct vnodeop_desc vop_vget_desc;
#define USES_VOP_VGET struct vop_vget_args vop_vget_a
#define VOP_VGET(A0,A1,A2) \
		( vop_vget_a.a_desc = VDESC(vop_vget), \
		vop_vget_a.a_mp=(A0), \
		vop_vget_a.a_ino=(A1), \
		vop_vget_a.a_vpp=(A2), \
		(VCALL((A0),VOFFSET(vop_vget),&vop_vget_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_valloc_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_pvp;
	int  a_mode;
	struct ucred * a_cred;
	struct vnode ** a_vpp;
};
extern struct vnodeop_desc vop_valloc_desc;
#define USES_VOP_VALLOC struct vop_valloc_args vop_valloc_a
#define VOP_VALLOC(A0,A1,A2,A3) \
		( vop_valloc_a.a_desc = VDESC(vop_valloc), \
		vop_valloc_a.a_pvp=(A0), \
		vop_valloc_a.a_mode=(A1), \
		vop_valloc_a.a_cred=(A2), \
		vop_valloc_a.a_vpp=(A3), \
		(VCALL((A0),VOFFSET(vop_valloc),&vop_valloc_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_vfree_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_pvp;
	ino_t  a_ino;
	int  a_mode;
};
extern struct vnodeop_desc vop_vfree_desc;
#define USES_VOP_VFREE struct vop_vfree_args vop_vfree_a
#define VOP_VFREE(A0,A1,A2) \
		( vop_vfree_a.a_desc = VDESC(vop_vfree), \
		vop_vfree_a.a_pvp=(A0), \
		vop_vfree_a.a_ino=(A1), \
		vop_vfree_a.a_mode=(A2), \
		(VCALL((A0),VOFFSET(vop_vfree),&vop_vfree_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_truncate_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_vp;
	u_long  a_length;
	int  a_flags;
	struct ucred * a_cred;
};
extern struct vnodeop_desc vop_truncate_desc;
#define USES_VOP_TRUNCATE struct vop_truncate_args vop_truncate_a
#define VOP_TRUNCATE(A0,A1,A2,A3) \
		( vop_truncate_a.a_desc = VDESC(vop_truncate), \
		vop_truncate_a.a_vp=(A0), \
		vop_truncate_a.a_length=(A1), \
		vop_truncate_a.a_flags=(A2), \
		vop_truncate_a.a_cred=(A3), \
		(VCALL((A0),VOFFSET(vop_truncate),&vop_truncate_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_update_args {
	struct vnodeop_desc *a_desc;
	struct vnode * a_vp;
	struct timeval * a_ta;
	struct timeval * a_tm;
	int  a_waitfor;
};
extern struct vnodeop_desc vop_update_desc;
#define USES_VOP_UPDATE struct vop_update_args vop_update_a
#define VOP_UPDATE(A0,A1,A2,A3) \
		( vop_update_a.a_desc = VDESC(vop_update), \
		vop_update_a.a_vp=(A0), \
		vop_update_a.a_ta=(A1), \
		vop_update_a.a_tm=(A2), \
		vop_update_a.a_waitfor=(A3), \
		(VCALL((A0),VOFFSET(vop_update),&vop_update_a)))

/* This is automatically generated.  Modify the corresponding .int file. */

struct vop_bwrite_args {
	struct vnodeop_desc *a_desc;
	struct buf * a_bp;
};
extern struct vnodeop_desc vop_bwrite_desc;
#define USES_VOP_BWRITE struct vop_bwrite_args vop_bwrite_a
#define VOP_BWRITE(A0) \
		( vop_bwrite_a.a_desc = VDESC(vop_bwrite), \
		vop_bwrite_a.a_bp=(A0), \
		(VCALL((A0),VOFFSET(vop_bwrite),&vop_bwrite_a)))

/* This is automatically generated.  Modify the corresponding .int file. */
/* BEGIN VERBATIM H */
extern struct vnodeop_desc vop_default_desc;
/* END VERBATIM H */
/* This is automatically generated.  Modify the corresponding .int file. */
/* BEGIN VERBATIM H */
/*
 * Three existing BSD vnodeops (strategy, vget and bwrite)
 * don't take any vnodes as arguments.
 * This means that these operations will never function successfully
 * through a bypass routine.
 * Both these operations are going away (right, Kirk?):
 *    vget will become a vfs operation
 *    bwrite and strategy
 *	will be replaced with an integrated VM page/buffer cache.
 *
 * To get around this problem for now we handle
 * these ops as special cases.
 */
#undef VOP_STRATEGY
#define VOP_STRATEGY(BP) \
		( vop_strategy_a.a_desc = VDESC(vop_strategy), \
		vop_strategy_a.a_bp=(BP), \
		(VCALL((BP)->b_vp,VOFFSET(vop_strategy),&vop_strategy_a)))
#undef VOP_BWRITE
#define VOP_BWRITE(BP) \
		( vop_bwrite_a.a_desc = VDESC(vop_bwrite), \
		vop_bwrite_a.a_bp=(BP), \
		(VCALL((BP)->b_vp,VOFFSET(vop_bwrite),&vop_bwrite_a)))
#undef VOP_VGET
#define VOP_VGET(VP,INO,VPP) \
		( vop_vget_a.a_desc = VDESC(vop_vget), \
		vop_vget_a.a_mp=(VP)->v_mount, \
		vop_vget_a.a_ino=(INO), \
		vop_vget_a.a_vpp=(VPP), \
		(VCALL((VP),VOFFSET(vop_vget),&vop_vget_a)))
/*
 * Many clients invoke vop_vget directly.  To aid them,
 * we define a few more macros.  (These go away
 * whe vget goes to the vfs interface.)
 */
#define LFS_VGET(MP,INO,VPP) \
		( vop_vget_a.a_desc = VDESC(vop_vget), \
		vop_vget_a.a_mp=(MP), \
		vop_vget_a.a_ino=(INO), \
		vop_vget_a.a_vpp=(VPP), \
		lfs_vget(&vop_vget_a))
#define FFS_VGET(MP,INO,VPP) \
		( vop_vget_a.a_desc = VDESC(vop_vget), \
		vop_vget_a.a_mp=(MP), \
		vop_vget_a.a_ino=(INO), \
		vop_vget_a.a_vpp=(VPP), \
		ffs_vget(&vop_vget_a))
/* END VERBATIM H */
