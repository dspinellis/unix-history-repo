/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.386.c%
 *
 *	@(#)conf.c	5.5 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "buf.h"
#include "ioctl.h"
#include "tty.h"
#include "conf.h"

int	nulldev();
int	nodev();
int	rawread(), rawwrite(), swstrategy();

#include "wd.h"
#if NWD > 0
int	wdopen(),wdclose(),wdstrategy(),wdread(),wdwrite(),wdioctl();
int	wddump(),wdsize();
#else
#define	wdopen		nodev
#define	wdclose		nodev
#define	wdstrategy	nodev
#define	wdread		nodev
#define	wdwrite		nodev
#define	wdioctl		nodev
#define	wddump		nodev
#define	wdsize		NULL
#endif

#include "xd.h"
#if NXD > 0
int	xdopen(),xdclose(),xdstrategy(),xdread(),xdwrite(),xdioctl();
int	xddump(),xdsize();
#else
#define	xdopen		nodev
#define	xdclose		nodev
#define	xdstrategy	nodev
#define	xdread		nodev
#define	xdwrite		nodev
#define	xdioctl		nodev
#define	xddump		nodev
#define	xdsize		NULL
#endif

#include "wt.h"
#if NWT > 0
int	wtopen(),wtclose(),wtstrategy(),wtread(),wtwrite(),wtioctl();
int	wtdump(),wtsize();
#else
#define	wtopen		nodev
#define	wtclose		nodev
#define	wtstrategy	nodev
#define	wtread		nodev
#define	wtwrite		nodev
#define	wtioctl		nodev
#define	wtdump		nodev
#define	wtsize		NULL
#endif

#include "fd.h"
#if NFD > 0
int	Fdopen(),fdclose(),fdstrategy(),fdread(),fdwrite();
#define	fdioctl		nodev
#define	fddump		nodev
#define	fdsize		NULL
#else
#define	Fdopen		nodev
#define	fdclose		nodev
#define	fdstrategy	nodev
#define	fdread		nodev
#define	fdwrite		nodev
#define	fdioctl		nodev
#define	fddump		nodev
#define	fdsize		NULL
#endif

int	swstrategy(),swread(),swwrite();

struct bdevsw	bdevsw[] =
{
	{ wdopen,	wdclose,	wdstrategy,	wdioctl,	/*0*/
	  wddump,	wdsize,		NULL },
	{ nodev,	nodev,		swstrategy,	nodev,		/*1*/
	  nodev,	nodev,		NULL },
	{ Fdopen,	fdclose,	fdstrategy,	fdioctl,	/*2*/
	  fddump,	fdsize,		NULL },
	{ wtopen,	wtclose,	wtstrategy,	wtioctl,	/*3*/
	  wtdump,	wtsize,		B_TAPE },
	{ xdopen,	xdclose,	xdstrategy,	xdioctl,	/*4*/
	  xddump,	xdsize,		NULL },
};
int	nblkdev = sizeof (bdevsw) / sizeof (bdevsw[0]);

int	cnopen(),cnclose(),cnread(),cnwrite(),cnioctl();
extern	struct tty cons;

int	syopen(),syread(),sywrite(),syioctl(),syselect();

int 	mmrw();
#define	mmselect	seltrue

#include "pty.h"
#if NPTY > 0
int	ptsopen(),ptsclose(),ptsread(),ptswrite(),ptsstop();
int	ptcopen(),ptcclose(),ptcread(),ptcwrite(),ptcselect();
int	ptyioctl();
struct	tty pt_tty[];
#else
#define ptsopen		nodev
#define ptsclose	nodev
#define ptsread		nodev
#define ptswrite	nodev
#define ptcopen		nodev
#define ptcclose	nodev
#define ptcread		nodev
#define ptcwrite	nodev
#define ptyioctl	nodev
#define	pt_tty		NULL
#define	ptcselect	nodev
#define	ptsstop		nulldev
#endif

#include "com.h"
#if NCOM > 0
int	comopen(),comclose(),comread(),comwrite(),comioctl();
#define comreset	nodev
extern	struct tty com_tty[];
#else
#define comopen		nodev
#define comclose	nodev
#define comread		nodev
#define comwrite	nodev
#define comioctl	nodev
#define comreset	nodev
#define	com_tty		NULL
#endif

int	logopen(),logclose(),logread(),logioctl(),logselect();

int	ttselect(), seltrue();


struct cdevsw	cdevsw[] =
{
	cnopen,		cnclose,	cnread,		cnwrite,	/*0*/
	cnioctl,	nulldev,	nulldev,	&cons,
	ttselect,	nodev,		NULL,
	syopen,		nulldev,	syread,		sywrite,	/*1*/
	syioctl,	nulldev,	nulldev,	NULL,
	syselect,	nodev,		NULL,
	nulldev,	nulldev,	mmrw,		mmrw,		/*2*/
	nodev,		nulldev,	nulldev,	NULL,
	mmselect,	nodev,		NULL,
	wdopen,		wdclose,	wdread,		wdwrite,	/*3*/
	wdioctl,	nodev,		nulldev,	NULL,
	seltrue,	nodev,		wdstrategy,
	nulldev,	nulldev,	rawread,	rawwrite,	/*4*/
	nodev,		nodev,		nulldev,	NULL,
	nodev,		nodev,		swstrategy,
	ptsopen,	ptsclose,	ptsread,	ptswrite,	/*5*/
	ptyioctl,	ptsstop,	nulldev,	pt_tty,
	ttselect,	nodev,		NULL,
	ptcopen,	ptcclose,	ptcread,	ptcwrite,	/*6*/
	ptyioctl,	nulldev,	nulldev,	pt_tty,
	ptcselect,	nodev,		NULL,
	logopen,	logclose,	logread,	nodev,		/*7*/
	logioctl,	nodev,		nulldev,	NULL,
	logselect,	nodev,		NULL,
	comopen,	comclose,	comread,	comwrite,	/*8*/
	comioctl,	nodev,		comreset,	com_tty,
	ttselect,	nodev,		NULL,
	Fdopen,		fdclose,	fdread,		fdwrite,	/*9*/
	fdioctl,	nodev,		nulldev,	NULL,
	seltrue,	nodev,		fdstrategy,
	wtopen,		wtclose,	wtread,		wtwrite,	/*A*/
	wtioctl,	nodev,		nulldev,	NULL,
	seltrue,	nodev,		wtstrategy,
	xdopen,		xdclose,	xdread,		xdwrite,	/*B*/
	xdioctl,	nodev,		nulldev,	NULL,
	seltrue,	nodev,		xdstrategy,
};
int	nchrdev = sizeof (cdevsw) / sizeof (cdevsw[0]);

int	mem_no = 2; 	/* major device number of memory special file */

/*
 * Swapdev is a fake device implemented
 * in sw.c used only internally to get to swstrategy.
 * It cannot be provided to the users, because the
 * swstrategy routine munches the b_dev and b_blkno entries
 * before calling the appropriate driver.  This would horribly
 * confuse, e.g. the hashing routines. Instead, /dev/drum is
 * provided as a character (raw) device.
 */
dev_t	swapdev = makedev(1, 0);
