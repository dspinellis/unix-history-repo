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
 *	@(#)os-aux.h	5.4 (Berkeley) %G%
 *
 * $Id: os-aux.h,v 5.2.2.1 1992/02/09 15:10:10 jsp beta $
 *
 * A/UX macII definitions for Amd (automounter)
 * Contributed by Julian Onions <jpo@cs.nott.ac.uk>
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
