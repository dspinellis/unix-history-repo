/*
 * Copyright (c) 1980, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)param.c	7.15 (Berkeley) %G%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980, 1986, 1989 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#include "../sys/param.h"
#include "../sys/systm.h"
#include "../sys/socket.h"
#include "../sys/user.h"
#include "../sys/proc.h"
#include "../sys/vnode.h"
#include "../sys/file.h"
#include "../sys/callout.h"
#include "../sys/clist.h"
#include "../sys/mbuf.h"
#include "../ufs/quota.h"
#include "../sys/kernel.h"
#ifdef SYSVSHM
#include "machine/vmparam.h"
#include "../sys/shm.h"
#endif

/*
 * System parameter formulae.
 *
 * This file is copied into each directory where we compile
 * the kernel; it should be modified there to suit local taste
 * if necessary.
 *
 * Compiled with -DHZ=xx -DTIMEZONE=x -DDST=x -DMAXUSERS=xx
 */

#ifndef HZ
#define	HZ 100
#endif
int	hz = HZ;
int	tick = 1000000 / HZ;
int	tickadj = 240000 / (60 * HZ);		/* can adjust 240ms in 60s */
struct	timezone tz = { TIMEZONE, DST };
#define	NPROC (20 + 8 * MAXUSERS)
int	nproc = NPROC;
#define NTEXT 100			/* actually the object cache */
#define NVNODE (NPROC + NTEXT + 300)
long	desiredvnodes = NVNODE;
int	nfile = 16 * (NPROC + 16 + MAXUSERS) / 10 + 32;
int	ncallout = 16 + NPROC;
int	nclist = 60 + 12 * MAXUSERS;
int     nmbclusters = NMBCLUSTERS;
int	fscale = FSCALE;	/* kernel uses `FSCALE', user uses `fscale' */

/*
 * Values in support of System V compatible shared memory.
 */
#ifdef SYSVSHM
#define	SHMMAX	(SHMMAXPGS*NBPG)
#define SHMMIN	1
#define SHMMNI	32			/* <= SHMMMNI in shm.h */
#define	SHMSEG	8
#define SHMALL	(SHMMAXPGS/CLSIZE)

struct	shminfo shminfo = {
	SHMMAX,
	SHMMIN,
	SHMMNI,
	SHMSEG,
	SHMALL
};
#endif

/*
 * These are initialized at bootstrap time
 * to values dependent on memory size
 */
int	nbuf, nswbuf;

/*
 * These have to be allocated somewhere; allocating
 * them here forces loader errors if this file is omitted
 * (if they've been externed everywhere else; hah!).
 */
struct	proc *proc, *procNPROC;
struct	file *file, *fileNFILE;
struct 	callout *callout;
struct	cblock *cfree;
struct	buf *buf, *swbuf;
char	*buffers;

/*
 * Proc/pgrp hashing.
 * Here so that hash table sizes can depend on MAXUSERS/NPROC.
 * Hash size must be a power of two.
 * NOW omission of this file will cause loader errors!
 */

#if NPROC > 1024
#define	PIDHSZ		512
#else
#if NPROC > 512
#define	PIDHSZ		256
#else
#if NPROC > 256
#define	PIDHSZ		128
#else
#define	PIDHSZ		64
#endif
#endif
#endif

struct	proc *pidhash[PIDHSZ];
struct	pgrp *pgrphash[PIDHSZ];
int	pidhashmask = PIDHSZ - 1;
