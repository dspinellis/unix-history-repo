/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)shm.h	8.1 (Berkeley) %G%
 */

/*
 * SVID compatible shm.h file
 */
#ifndef _SHM_H_
#define _SHM_H_

#ifdef KERNEL
#include "ipc.h"
#else
#include <sys/ipc.h>
#endif

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

#endif /* !_SHM_H_ */
