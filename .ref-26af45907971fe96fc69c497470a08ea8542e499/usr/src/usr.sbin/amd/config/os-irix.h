/*
 * Copyright (c) 1990 Jan-Simon Pendry
 * Copyright (c) 1990 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)os-irix.h	5.4 (Berkeley) %G%
 *
 * $Id: os-irix.h,v 5.2.2.1 1992/02/09 15:10:28 jsp beta $
 *
 * IRIX 3.3 definitions for Amd (automounter)
 * Contributed by Scott R. Presnell <srp@cgl.ucsf.edu>
 */

/*
 * Does the compiler grok void *
 */
#define VOIDP

/*
 * Which version of the Sun RPC library we are using
 * This is the implementation release number, not
 * the protocol revision number.
 */
#define RPC_3

/*
 * Which version of the NFS interface are we using.
 * This is the implementation release number, not
 * the protocol revision number.
 */
#define NFS_3

/*
 * Byte ordering
 */
#undef ARCH_ENDIAN
#define ARCH_ENDIAN	"big"

/*
 * Has support for syslog()
 */
#define HAS_SYSLOG

#define M_GRPID		MS_GRPID
#define M_RDONLY	MS_RDONLY
/*
 * Support for ndbm
 */
#define OS_HAS_NDBM

#define UPDATE_MTAB

#undef	MTAB_TYPE_NFS
#define MTAB_TYPE_NFS	"nfs"

#undef	MTAB_TYPE_UFS
#define MTAB_TYPE_UFS	"efs"

#define NMOUNT	40	/* The std sun value */
/*
 * Name of filesystem types
 */
#define MOUNT_TYPE_UFS	sysfs(GETFSIND, FSID_EFS)
#define MOUNT_TYPE_NFS	sysfs(GETFSIND, FSID_NFS)

#define SYS5_SIGNALS

/*
 * Use <fcntl.h> rather than <sys/file.h>
 */
/*#define USE_FCNTL*/

/*
 * Use fcntl() rather than flock()
 */
/*#define LOCK_FCNTL*/

#ifdef __GNUC__
#define alloca(sz) __builtin_alloca(sz)
#endif

#define bzero(ptr, len) memset(ptr, 0, len)
#define bcopy(from, to, len) memcpy(to, from, len)

#undef MOUNT_TRAP
#define MOUNT_TRAP(type, mnt, flags, mnt_data) \
	irix_mount(mnt->mnt_fsname, mnt->mnt_dir,flags, type, mnt_data)
#undef UNMOUNT_TRAP
#define UNMOUNT_TRAP(mnt)	umount(mnt->mnt_dir)
#define NFDS	30	/* conservative */

#define NFS_HDR "misc-irix.h"
#define UFS_HDR "misc-irix.h"

/* not included in sys/param.h */
#include <sys/types.h>

#define MOUNT_HELPER_SOURCE "mount_irix.c"

#define	MNTINFO_DEV	"fsid"
#define	MNTINFO_PREF	"0x"
