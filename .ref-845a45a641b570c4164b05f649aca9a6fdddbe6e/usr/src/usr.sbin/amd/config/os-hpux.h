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
 *	@(#)os-hpux.h	5.4 (Berkeley) %G%
 *
 * $Id: os-hpux.h,v 5.2.2.1 1992/02/09 15:10:23 jsp beta $
 *
 * HP/9000 HP-UX definitions for Amd (automounter)
 */

/*
 * Does the compiler grok void *
 */
#ifdef __GNUC__
#define	VOIDP
#endif

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
#if defined(hp9000s200) || defined(hp9000s300) || defined(hp9000s800)
#define	ARCH_ENDIAN	"big"
#endif

#ifndef __hpux
#define	HPUX_VERSION_6
#endif

/*
 * No support for syslog() prior to 7.0
 */
#ifdef HPUX_VERSION_6
#undef HAS_SYSLOG
#endif

/*
 * No support for ndbm
 */
#undef OS_HAS_NDBM

/*
 * Name of filesystem types
 */
#define	MOUNT_TYPE_UFS	MOUNT_UFS
#define MOUNT_TYPE_NFS MOUNT_NFS
#undef MTAB_TYPE_UFS
#define	MTAB_TYPE_UFS	"hfs"

/*
 * Where to get NFS definitions
 */
#define	NFS_HDR "misc-hpux.h"

/*
 * Where to get union wait
 */
#undef WAIT
#define	WAIT	"uwait.h"
#ifdef HPUX_VERSION_6
#define SIGCHLD	SIGCLD
#endif
#define	SYS5_SIGNALS

/*
 * Miscellaneous HP-UX definitions
 */

#define NEED_XDR_POINTER
#define	NEED_CLNT_SPERRNO

/*
 * Use <fcntl.h> rather than <sys/file.h>
 */
#define USE_FCNTL

/*
 * Use fcntl() rather than flock()
 */
#define LOCK_FCNTL

/*
 * Additional fields in struct mntent
 * are fixed up here
 */
#define FIXUP_MNTENT(mntp) { \
	(mntp)->mnt_time = clocktime(); \
}
#define FIXUP_MNTENT_DUP(mntp, mp) { \
	(mntp)->mnt_time = (mp)->mnt_time; \
}

#define	bzero(ptr, len)	memset(ptr, 0, len)
#define bcopy(from, to, len) memcpy(to, from, len)
#define getpagesize() (2048)
#undef MOUNT_TRAP
#define MOUNT_TRAP(type, mnt, flags, mnt_data) \
	vfsmount(type, mnt->mnt_dir, flags, mnt_data)
#undef UNMOUNT_TRAP
#define	UNMOUNT_TRAP(mnt)	umount(mnt->mnt_dir)
#define NFDS	30	/* conservative */
#define	MOUNTED MNT_MNTTAB
