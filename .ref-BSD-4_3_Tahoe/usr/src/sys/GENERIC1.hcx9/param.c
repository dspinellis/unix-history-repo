/*
 * Copyright (c) 1980, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)param.c	7.3 (Berkeley) 12/6/86
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980, 1986 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/socket.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/text.h"
#include "../h/inode.h"
#include "../h/file.h"
#include "../h/callout.h"
#include "../h/clist.h"
#include "../h/cmap.h"
#include "../h/mbuf.h"
#include "../h/quota.h"
#include "../h/kernel.h"
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
int	ntext = 36 + MAXUSERS;
#define NINODE ((NPROC + 16 + MAXUSERS) + 32)
int	ninode = NINODE;
int	nchsize = NINODE * 11 / 10;
int	nfile = 16 * (NPROC + 16 + MAXUSERS) / 10 + 32;
int	ncallout = 16 + NPROC;
int	nclist = 60 + 12 * MAXUSERS;
int     nmbclusters = NMBCLUSTERS;
#ifdef QUOTA
int	nquota = (MAXUSERS * 9) / 7 + 3;
int	ndquot = NINODE + (MAXUSERS * NMOUNT) / 4;
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
struct	text *text, *textNTEXT;
struct	inode *inode, *inodeNINODE;
struct	file *file, *fileNFILE;
struct 	callout *callout;
struct	cblock *cfree;
struct	buf *buf, *swbuf;
char	*buffers;
struct	cmap *cmap, *ecmap;
struct	namecache *namecache;
#ifdef QUOTA
struct	quota *quota, *quotaNQUOTA;
struct	dquot *dquot, *dquotNDQUOT;
#endif
