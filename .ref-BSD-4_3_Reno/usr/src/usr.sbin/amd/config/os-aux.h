/* $Id: os-aux.h,v 5.2 90/06/23 22:20:37 jsp Rel $ */

/*
 * A/UX macII definitions for Amd (automounter)
 * Contributed by Julian Onions <jpo@cs.nott.ac.uk>
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
 *	@(#)os-aux.h	5.1 (Berkeley) 6/29/90
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
#define	ARCH_ENDIAN	"big"

/*
 * Has support for syslog()
 */
#define HAS_SYSLOG

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
#define	MTAB_TYPE_UFS	"5.2"

#define SIGCHLD	SIGCLD
#define	SYS5_SIGNALS

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
	fsmount(type, mnt->mnt_dir, flags, mnt_data)
#undef UNMOUNT_TRAP
#define	UNMOUNT_TRAP(mnt)	unmount(mnt->mnt_dir)
#define NFDS	30	/* conservative */

/* not included in sys/param.h */
#include <sys/types.h>
/* not part of sys/time.h */
#include <time.h>
/* for NMOUNT */
#include <sys/config.h>
