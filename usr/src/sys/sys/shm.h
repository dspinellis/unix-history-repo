/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 * (c) UNIX System Laboratories, Inc.
 * All or some portions of this file are derived from material licensed
 * to the University of California by American Telephone and Telegraph
 * Co. or Unix System Laboratories, Inc. and are reproduced herein with
 * the permission of UNIX System Laboratories, Inc.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.  Originally from the University of Wisconsin.
 *
 * %sccs.include.proprietary.c%
 *
 *	@(#)shm.h	8.6 (Berkeley) %G%
 */

/*
 * SVID compatible shm.h file
 */
#ifndef _SYS_SHM_H_
#define _SYS_SHM_H_

#include <sys/ipc.h>

struct shmid_ds {
	struct	ipc_perm shm_perm;	/* operation perms */
	int	shm_segsz;		/* size of segment (bytes) */
	ushort	shm_cpid;		/* pid, creator */
	ushort	shm_lpid;		/* pid, last operation */
	short	shm_nattch;		/* no. of current attaches */
	time_t	shm_atime;		/* last attach time */
	time_t	shm_dtime;		/* last detach time */
	time_t	shm_ctime;		/* last change time */
	void	*shm_handle;		/* internal handle for shm segment */
};

/*
 * System 5 style catch-all structure for shared memory constants that
 * might be of interest to user programs.  Do we really want/need this?
 */
struct	shminfo {
	int	shmmax;		/* max shared memory segment size (bytes) */
	int	shmmin;		/* min shared memory segment size (bytes) */
	int	shmmni;		/* max number of shared memory identifiers */
	int	shmseg;		/* max shared memory segments per process */
	int	shmall;		/* max amount of shared memory (pages) */
};

/* internal "mode" bits */
#define	SHM_ALLOC	01000	/* segment is allocated */
#define	SHM_DEST	02000	/* segment will be destroyed on last detach */

/* SVID required constants (same values as system 5) */
#define	SHM_RDONLY	010000	/* read-only access */
#define	SHM_RND		020000	/* round attach address to SHMLBA boundary */

/* implementation constants */
#define	SHMLBA		CLBYTES	/* segment low boundary address multiple */
#define	SHMMMNI		512	/* maximum value for shminfo.shmmni */

#ifdef KERNEL
struct	shmid_ds	*shmsegs;
struct	shminfo		shminfo;
#endif

#endif /* !_SYS_SHM_H_ */
