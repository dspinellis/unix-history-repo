/* $Id: os-aix3.h,v 5.2 90/06/23 22:20:33 jsp Rel $ */

/*
 * AIX 3.1 definitions for Amd (automounter)
 *
 * Copyright (c) 1990 Jan-Simon Pendry
 * Copyright (c) 1990 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)os-aix3.h	5.1 (Berkeley) 6/29/90
 */

/*
 * Does the compiler grok void *
 */
#define	VOIDP

/*
 * Which version of the Sun RPC library we are using
 * This is the implementation release number, not
 * the protocol revision number.
 */
#define	RPC_4

/*
 * Which version of the NFS interface are we using.
 * This is the implementation release number, not
 * the protocol revision number.
 */
#define	NFS_AIX3

/*
 * Does this OS have NDBM support?
 */
#define OS_HAS_NDBM

/*
 * The mount table is obtained from the kernel
 */
#undef	UPDATE_MTAB

/*
 * Pick up BSD bits from include files
 */
#define	_BSD

/*
 * No mntent info on AIX 3
 */
#undef	MNTENT_HDR
#define	MNTENT_HDR <sys/mntctl.h>

/*
 * Name of filesystem types
 */
#define	MOUNT_TYPE_NFS	MNT_NFS
#define	MOUNT_TYPE_UFS	MNT_JFS
#undef MTAB_TYPE_UFS
#define	MTAB_TYPE_UFS	"jfs"

/*
 * How to unmount filesystems
 */
#undef MOUNT_TRAP
#define	MOUNT_TRAP(type, mnt, flag, mnt_data) \
	aix3_mount(mnt->mnt_fsname, mnt->mnt_dir, flag, type, mnt_data, mnt->mnt_opts)
#undef	UNMOUNT_TRAP
#define	UNMOUNT_TRAP(mnt)	uvmount(mnt->mnt_passno, 0)


/*
 * Byte ordering
 */
#ifndef BYTE_ORDER
#include <sys/machine.h>
#endif /* BYTE_ORDER */

#undef ARCH_ENDIAN
#if BYTE_ORDER == LITTLE_ENDIAN
#define ARCH_ENDIAN "little"
#else
#if BYTE_ORDER == BIG_ENDIAN
#define ARCH_ENDIAN "big"
#else
XXX - Probably no hope of running Amd on this machine!
#endif /* BIG */
#endif /* LITTLE */

/*
 * Miscellaneous AIX 3 bits
 */
#define	NEED_MNTOPT_PARSER
#define	SHORT_MOUNT_NAME

#define	MNTMAXSTR       128

#define	MNTTYPE_UFS	"jfs"		/* Un*x file system */
#define	MNTTYPE_NFS	"nfs"		/* network file system */
#define	MNTTYPE_IGNORE	"ignore"	/* No type specified, ignore this entry */

struct mntent {
	char	*mnt_fsname;	/* name of mounted file system */
	char	*mnt_dir;	/* file system path prefix */
	char	*mnt_type;	/* MNTTYPE_* */
	char	*mnt_opts;	/* MNTOPT* */
	int	mnt_freq;	/* dump frequency, in days */
	int	mnt_passno;	/* pass number on parallel fsck */
};

#define	NFS_HDR "misc-aix3.h"
#define	UFS_HDR "misc-aix3.h"
#undef NFS_FH_DREF
#define	NFS_FH_DREF(dst, src) { (dst).addr = *(src); }
#undef NFS_SA_DREF
#define	NFS_SA_DREF(dst, src) { (dst).addr = *(src); }
#define	M_RDONLY MNT_READONLY

/*
 * How to get a mount list
 */
#undef	READ_MTAB_FROM_FILE
#define	READ_MTAB_AIX3_STYLE

/*
 * The data for the mount syscall needs the path in addition to the
 * host name since that is the only source of information about the
 * mounted filesystem.
#define	NFS_ARGS_NEEDS_PATH
 */

#define	NFS_LOMAP	34
#define	NFS_HIMAP	99
#define NFS_ERROR_MAPPING \
static nfs_errormap[] = {	     0,75,77,99,99,99, \
			99,99,99,99,99,78,99,99,99,79, \
			99,99,70,99,35,36,37,38,39,40, \
			41,42,43,44,45,46,47,48,49,50, \
			51,52,53,54,55,56,57,58,60,61, \
			64,65,99,67,68,62,63,66,69,68, \
			99,99,99,71,99,99,99,99,99,99 \
			};

#define	MOUNT_AIX3

/*
 * Need this too
 */
#include <time.h>
