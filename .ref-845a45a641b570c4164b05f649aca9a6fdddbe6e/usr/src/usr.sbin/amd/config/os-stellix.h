/*
 * Copyright (c) 1989 Jan-Simon Pendry
 * Copyright (c) 1989 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)os-stellix.h	5.2 (Berkeley) %G%
 *
 * Amd (automounter) definitions for Stellix.
 * From Stephen C. Pope <scp@acl.lanl.gov>
 *
 * $Id: os-stellix.h,v 5.2.2.1 1992/02/09 15:10:43 jsp beta $
 */

#define RPC_3

#define NFS_3

/*
 * Byte ordering
 */
#undef ARCH_ENDIAN
#define	ARCH_ENDIAN	"big"

#define HAS_SYSLOG

#define OS_HAS_NDBM

#define UPDATE_MTAB

#define USE_FCNTL

#define LOCK_FCNTL

/*
 * Name of filesystem types
 */
#undef MTAB_TYPE_UFS
#define	MTAB_TYPE_UFS	"sfs"

#define MOUNT_TYPE_UFS sysfs(GETFSIND, "SFS1")
#define MOUNT_TYPE_NFS sysfs(GETFSIND, "NFS")

#define SYS5_SIGNALS
#define HAS_SVR3_SIGNALS

#define MOUNT_HELPER_SOURCE "mount_stellix.c"

/*
 * Name of mount & unmount system calls
 *
 * NOTE:
 *  UNMOUNT_TRAP takes a struct mntent *
 */
#undef MOUNT_TRAP
#define	MOUNT_TRAP(type, mnt, flags, mnt_data) \
	stellix_mount(mnt->mnt_fsname, mnt->mnt_dir, flags, type, mnt_data)
#undef UNMOUNT_TRAP
#define	UNMOUNT_TRAP(mnt)	umount(mnt->mnt_dir)

/*
 * How to unmount filesystems.
 * NEED_UMOUNT_FS includes code to scan the mount table
 * to find the correct information for the unmount system
 * call.  Some systems, such as 4.4bsd, do not require
 * this - they can just do an unmount system call directly.
 */
/* #define	NEED_UMOUNT_FS */
/* #define	UMOUNT_FS(dir)	umount_fs(dir) */

#define NFS_HDR "misc-stellix.h"
#define UFS_HDR "misc-stellix.h"

#define M_RDONLY        0x01            /* mount fs read only */

#define bzero(ptr, len) memset(ptr, 0, len)
#define bcopy(from, to, len) memcpy(to, from, len)
