/* $Id: os-hpux.h,v 5.2 90/06/23 22:20:47 jsp Rel $ */

/*
 * HP/9000 HP-UX definitions for Amd (automounter)
 *
 * Copyright (c) 1989 Jan-Simon Pendry
 * Copyright (c) 1989 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1989 The Regents of the University of California.
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
 *	@(#)os-hpux.h	5.1 (Berkeley) 6/29/90
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
#if defined(hp9000s200) || defined(hp9000s300)
#define	ARCH_ENDIAN	"big"
#endif
/*
#if defined(hp9000s800)
#define ARCH_ENDIAN	"unknown"
#endif
*/

/*
 * No support for syslog()
 */
#undef HAS_SYSLOG

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
#ifndef SIGCHLD
#define SIGCHLD	SIGCLD
#endif
#define	SYS5_SIGNALS

/*
 * Miscellaneous HP-UX definitions
 */

#define	MISC_RPC

/*
 * Use <fcntl.h> rather than <sys/file.h>
 */
#define USE_FCNTL

/*
 * Use fcntl() rather than flock()
 */
#define LOCK_FCNTL

#ifdef __GNUC__
#define alloca(sz) __builtin_alloca(sz)
#endif
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
