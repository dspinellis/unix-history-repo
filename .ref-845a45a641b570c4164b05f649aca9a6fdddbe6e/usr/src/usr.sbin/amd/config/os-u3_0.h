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
 *	@(#)os-u3_0.h	5.4 (Berkeley) %G%
 *
 * $Id: os-u3_0.h,v 5.2.2.1 1992/02/09 15:10:52 jsp beta $
 *
 * Ultrix 3.0 definitions for Amd (automounter)
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
#undef ARCH_ENDIAN
#if defined(vax) || defined(mips)
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

#define	MNTMAXSTR	128

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

#define NEED_XDR_POINTER
#define	NEED_CLNT_SPERRNO

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
