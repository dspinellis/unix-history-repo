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
 *	@(#)os-dgux.h	5.4 (Berkeley) %G%
 *
 * $Id: os-dgux.h,v 5.2.2.1 1992/02/09 15:10:18 jsp beta $
 *
 * dg/ux definitions for Amd (automounter)
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
#define RPC_4

/*
 * Which version of the NFS interface are we using.
 * This is the implementation release number, not
 * the protocol revision number.
 */
#define NFS_4

/*
 * Name of filesystem types
 */
#define MOUNT_TYPE_NFS	"nfs"
#define MOUNT_TYPE_UFS	"dg/ux"
#undef MTAB_TYPE_UFS
#define MTAB_TYPE_UFS	"dg/ux"

/*
 * Need the following in more places than just NFS_HDR
 */
#include <sys/dg_mount.h>
/*
 * This is braindead
 * dg/ux has nfs 4.0 but doesn't have the following options
 */
#define NFSMNT_HOSTNAME 0x0
#define NFSMNT_INT 0x0
#define M_NEWTYPE 0

/*
 * DG have their own filesystem.
 */
#define ufs_args dgux_args

/*
 * Byte ordering
 */

#undef ARCH_ENDIAN
#define ARCH_ENDIAN "big"

#define _BSD_WAIT_FLAVOR
#define _BSD_TTY_FLAVOR
#define _BSD_SIGNAL_FLAVOR
#define _DGUX_SOURCE

/*
 * Use fcntl() rather than flock()
 */
#define LOCK_FCNTL

#define bzero(ptr, len) memset(ptr, 0, len)
#define bcopy(from, to, len) memcpy(to, from, len)
#undef MOUNT_TRAP
#define MOUNT_TRAP(type, mnt, flags, mnt_data) \
	((struct nfs_args *)mnt_data)->version = !strcmp(type, MOUNT_TYPE_UFS)?\
	     DG_MOUNT_DGUX_VERSION:DG_MOUNT_NFS_VERSION, \
	dg_mount(type, mnt->mnt_dir, flags, mnt_data)
#undef UNMOUNT_TRAP
#define UNMOUNT_TRAP(mnt)	umount(mnt->mnt_dir)
