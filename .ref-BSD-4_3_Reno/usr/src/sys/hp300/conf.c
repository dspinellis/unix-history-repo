/*
 * Copyright (c) 1982, 1990 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)conf.c	7.1 (Berkeley) 5/8/90
 */

#include "param.h"
#include "systm.h"
#include "buf.h"
#include "ioctl.h"
#include "tty.h"
#include "conf.h"

int	nulldev(), nodev(), rawread(), rawwrite(), swstrategy();

#include "ct.h"
#if NCT > 0
int	ctopen(),ctclose(),ctstrategy(),ctread(),ctwrite(),ctdump(),ctioctl();
#else
#define	ctopen		nodev
#define	ctclose		nodev
#define	ctstrategy	nodev
#define	ctread		nodev
#define	ctwrite		nodev
#define	ctdump		nodev
#define	ctioctl		nodev
#endif
 
#include "rd.h"
#if NRD > 0
int	rdopen(),rdstrategy(),rdread(),rdwrite(),rddump(),rdioctl(),rdsize();
#else
#define	rdopen		nodev
#define	rdstrategy	nodev
#define	rdread		nodev
#define	rdwrite		nodev
#define	rddump		nodev
#define	rdioctl		nodev
#define	rdsize		0
#endif
 
#include "sd.h"
#if NSD > 0
int	sdopen(),sdstrategy(),sdread(),sdwrite(),sddump(),sdioctl(),sdsize();
#else
#define	sdopen		nodev
#define	sdstrategy	nodev
#define	sdread		nodev
#define	sdwrite		nodev
#define	sddump		nodev
#define	sdioctl		nodev
#define	sdsize		0
#endif
 
#include "cd.h"
#if NCD > 0
int	cdopen(),cdstrategy(),cdread(),cdwrite(),cddump(),cdioctl(),cdsize();
#else
#define	cdopen		nodev
#define	cdstrategy	nodev
#define	cdread		nodev
#define	cdwrite		nodev
#define	cddump		nodev
#define	cdioctl		nodev
#define	cdsize		0
#endif
 
#include "fd.h"
#if NFD > 0
int	fdopen(),fdstrategy(),fdread(),fdwrite(),fddump(),fdioctl(),fdsize();
#else
#define	fdopen		nodev
#define	fdstrategy	nodev
#define	fdread		nodev
#define	fdwrite		nodev
#define	fddump		nodev
#define	fdioctl		nodev
#define	fdsize		0
#endif

struct bdevsw	bdevsw[] =
{
	{ ctopen,	ctclose,	ctstrategy,	ctioctl,	/*0*/
	  ctdump,	0,		B_TAPE },
	{ nodev,	nodev,		nodev,		nodev,		/*1*/
	  nodev,	0,		0 },
	{ rdopen,	nulldev,	rdstrategy,	rdioctl,	/*2*/
	  rddump,	rdsize,		0 },
	{ nodev,	nodev,		swstrategy,	nodev,		/*3*/
	  nodev,	0,		0 },
	{ sdopen,	nulldev,	sdstrategy,	sdioctl,	/*4*/
	  sddump,	sdsize,		0 },
	{ cdopen,	nulldev,	cdstrategy,	cdioctl,	/*5*/
	  cddump,	cdsize,		0 },
	{ fdopen,	nulldev,	fdstrategy,	fdioctl,	/*6*/
	  fddump,	fdsize,		0 },
};
int	nblkdev = sizeof (bdevsw) / sizeof (bdevsw[0]);

int	cnopen(),cnclose(),cnread(),cnwrite(),cnioctl(),cnselect();

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
#define	pt_tty		0
#define	ptcselect	nodev
#define	ptsstop		nulldev
#endif

#include "ppi.h"
#if NPPI > 0
int	ppiopen(),ppiclose(),ppiread(),ppiwrite();
#else
#define ppiopen		nodev
#define ppiclose	nodev
#define ppiread		nodev
#define ppiwrite	nodev
#endif

#include "ite.h"
#if NITE > 0
int	iteopen(),iteclose(),iteread(),itewrite(),iteioctl();
struct	tty ite_tty[];
#else
#define iteopen		nodev
#define iteclose	nodev
#define iteread		nodev
#define itewrite	nodev
#define itestop		nulldev
#define iteioctl	nodev
#define ite_tty		0
#endif

#include "dca.h"
#if NDCA > 0
int	dcaopen(),dcaclose(),dcaread(),dcawrite(),dcastop(),dcaioctl();
struct	tty dca_tty[];
#else
#define dcaopen		nodev
#define dcaclose	nodev
#define dcaread		nodev
#define dcawrite	nodev
#define dcastop		nulldev
#define dcaioctl	nodev
#define dca_tty		0
#endif

#include "dcm.h"
#if NDCM > 0
int	dcmopen(),dcmclose(),dcmread(),dcmwrite(),dcmstop(),dcmioctl();
struct	tty dcm_tty[];
#else
#define dcmopen		nodev
#define dcmclose	nodev
#define dcmread		nodev
#define dcmwrite	nodev
#define dcmstop		nulldev
#define dcmioctl	nodev
#define dcm_tty		0
#endif

#include "clock.h"
#if NCLOCK > 0
int	clockopen(),clockclose(),clockioctl(),clockmap();
#else
#define clockopen	nodev
#define clockclose	nodev
#define clockioctl	nodev
#define clockmap	nodev
#endif

int	logopen(),logclose(),logread(),logioctl(),logselect();

int	ttselect(), seltrue();

int	grfopen(),grfclose(),grfioctl(),grfselect(),grfmap();

int	hilopen(),hilclose(),hilioctl(),hilmap(),hilselect(),hilread();

struct cdevsw	cdevsw[] =
{
	cnopen,		cnclose,	cnread,		cnwrite,	/*0*/
	cnioctl,	nulldev,	nulldev,	NULL,
	cnselect,	nodev,		NULL,
	syopen,		nulldev,	syread,		sywrite,	/*1*/
	syioctl,	nulldev,	nulldev,	NULL,
	syselect,	nodev,		NULL,
	nulldev,	nulldev,	mmrw,		mmrw,		/*2*/
	nodev,		nulldev,	nulldev,	NULL,
	mmselect,	nodev,		NULL,
	nulldev,	nulldev,	rawread,	rawwrite,	/*3*/
	nodev,		nodev,		nulldev,	NULL,
	nodev,		nodev,		swstrategy,
	ptsopen,	ptsclose,	ptsread,	ptswrite,	/*4*/
	ptyioctl,	ptsstop,	nulldev,	pt_tty,
	ttselect,	nodev,		NULL,
	ptcopen,	ptcclose,	ptcread,	ptcwrite,	/*5*/
	ptyioctl,	nulldev,	nulldev,	pt_tty,
	ptcselect,	nodev,		NULL,
	logopen,	logclose,	logread,	nodev,		/*6*/
	logioctl,	nodev,		nulldev,	NULL,
	logselect,	nodev,		NULL,
	ctopen,		ctclose,	ctread,		ctwrite,	/*7*/
	ctioctl,	nodev,		nulldev,	NULL,
	seltrue,	nodev,		ctstrategy,
	sdopen,		nulldev,	sdread,		sdwrite,	/*8*/
	sdioctl,	nodev,		nulldev,	NULL,
	seltrue,	nodev,		sdstrategy,
	rdopen,		nulldev,	rdread,		rdwrite,	/*9*/
	rdioctl,	nodev,		nulldev,	NULL,
	seltrue,	nodev,		rdstrategy,
	grfopen,	grfclose,	nulldev,	nulldev,	/*10*/
	grfioctl,	nodev,		nulldev,	NULL,
	grfselect,	grfmap,		NULL,
	ppiopen,	ppiclose,	ppiread,	ppiwrite,	/*11*/
	nodev,		nodev,		nulldev,	NULL,
	nodev,		nodev,		NULL,
	dcaopen,	dcaclose,	dcaread,	dcawrite,	/*12*/
	dcaioctl,	dcastop,	nulldev,	dca_tty,
	ttselect,	nodev,		NULL,
	iteopen,	iteclose,	iteread,	itewrite,	/*13*/
	iteioctl,	nodev,		nulldev,	ite_tty,
	ttselect,	nodev,		NULL,
	hilopen,	hilclose,	hilread,	nulldev,	/*14*/
	hilioctl,	nodev,		nulldev,	NULL,
	hilselect,	hilmap,		NULL,
	dcmopen,	dcmclose,	dcmread,	dcmwrite,	/*15*/
	dcmioctl,	dcmstop,	nulldev,	dcm_tty,
	ttselect,	nodev,		NULL,
	nodev,		nodev,		nodev,		nodev,		/*16*/
	nodev,		nodev,		nulldev,	NULL,
	seltrue,	nodev,		NULL,
	cdopen,		nulldev,	cdread,		cdwrite,	/*17*/
	cdioctl,	nodev,		nulldev,	NULL,
	seltrue,	nodev,		cdstrategy,
	clockopen,	clockclose,	nulldev,	nulldev,	/*18*/
	clockioctl,	nodev,		nulldev,	NULL,
	nulldev,	clockmap,	NULL,
	fdopen,		nulldev,	fdread,		fdwrite,	/*19*/
	fdioctl,	nodev,		nulldev,	NULL,
	seltrue,	nodev,		NULL,
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
dev_t	swapdev = makedev(3, 0);
