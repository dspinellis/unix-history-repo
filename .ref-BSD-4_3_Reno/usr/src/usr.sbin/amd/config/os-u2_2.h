/* $Id: os-u2_2.h,v 5.2 90/06/23 22:20:55 jsp Rel $ */

/*
 * Ultrix 2.2 definitions for Amd (automounter)
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
 *	@(#)os-u2_2.h	5.1 (Berkeley) 6/29/90
 */

/*
 * Does the compiler grok void *
 */
#undef	VOIDP

/*
 * Which version of the Sun RPC library we are using
 * This is the implementation release number, not
 * the protocol revision number.
 */
#define	RPC_3

/*
 * Which version of the NFS interface are we using.
 * This is the implementation release number, not
 * the protocol revision number.
 */
#define	NFS_3

/*
 * Byte ordering
 */
#undef	ARCH_ENDIAN
#if defined(vax)
#define	ARCH_ENDIAN "little"
#endif

/*
 * The mount table is obtained from the kernel
 */
#undef	UPDATE_MTAB

/*
 * No mntent info on Ultrix
  */
#undef	MNTENT_HDR

/*
 * No support for syslog()
 */
#undef	HAS_SYSLOG

/*
 * No support for ndbm
 */
#undef	HAS_NDBM_MAPS

/*
 * Name of filesystem types
 */
#define	MOUNT_TYPE_NFS	GT_NFS
#define	MOUNT_TYPE_UFS	GT_ULTRIX
#undef	MTAB_TYPE_UFS
#define	MTAB_TYPE_UFS	"ufs"

/*
 * Name of mount & unmount system calls
 */
#undef	MOUNT_TRAP
#define	MOUNT_TRAP(type, mnt, flag, mnt_data) \
	mount(mnt->mnt_fsname, mnt->mnt_dir, flag, type, mnt_data)
#undef	UNMOUNT_TRAP
#define	UNMOUNT_TRAP(mnt)	umount(mnt->mnt_passno)

/*
 * Miscellaneous Ultrix bits
 */
#define	M_RDONLY	M_RONLY

#ifndef MNTMAXSTR
#define	MNTMAXSTR       128
#endif

#define	MNTTYPE_UFS	"ufs"		/* Un*x file system */
#define	MNTTYPE_NFS	"nfs"		/* network file system */
#define	MNTTYPE_IGNORE	"ignore"	/* No type specified, ignore this entry */

#define	MNTOPT_RO	"ro"		/* read only */
#define	MNTOPT_RW	"rw"		/* read/write */
#define	MNTOPT_QUOTA	"quota"		/* quotas */
#define	MNTOPT_NOQUOTA	"noquota"	/* no quotas */
#define	MNTOPT_HARD	"hard"		/* hard mount */
#define	MNTOPT_SOFT	"soft"		/* soft mount */
#define	MNTOPT_INTR	"intr"		/* interrupts allowed */

#define	MNTOPT_NOSUID	"nosuid"	/* no set uid allowed */

struct mntent {
	char	*mnt_fsname;	/* name of mounted file system */
	char	*mnt_dir;	/* file system path prefix */
	char	*mnt_type;	/* MNTTYPE_* */
	char	*mnt_opts;	/* MNTOPT* */
	int	mnt_freq;	/* dump frequency, in days */
	int	mnt_passno;	/* pass number on parallel fsck */
};
#define	MOUNTED		"/etc/mtab"

#define	NFS_HDR	"misc-ultrix.h"
#define	UFS_HDR	"misc-ultrix.h"

#define	MISC_RPC

#define	nfs_args	nfs_gfs_mount
#define	ULTRIX_HACK	/* Should be handled better than this !! */
#define	NEED_MNTOPT_PARSER

/*
 * How to get a mount list
 */
#undef	READ_MTAB_FROM_FILE
#define	READ_MTAB_ULTRIX_STYLE

/*
 * Need precise length links
 */
#define	PRECISE_SYMLINKS
