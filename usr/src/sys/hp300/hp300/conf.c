/*
 * Copyright (c) 1982, 1990 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)conf.c	7.6 (Berkeley) %G%
 */

#include "sys/param.h"
#include "sys/systm.h"
#include "sys/buf.h"
#include "sys/ioctl.h"
#include "sys/tty.h"
#include "sys/conf.h"

int	nullop(), enxio(), enodev(), rawread(), rawwrite(), swstrategy();

#include "ct.h"
#if NCT > 0
int	ctopen(),ctclose(),ctstrategy(),ctread(),ctwrite(),ctdump(),ctioctl();
#else
#define	ctopen		enxio
#define	ctclose		enxio
#define	ctstrategy	enxio
#define	ctread		enxio
#define	ctwrite		enxio
#define	ctdump		enxio
#define	ctioctl		enxio
#endif
 
#include "rd.h"
#if NRD > 0
int	rdopen(),rdstrategy(),rdread(),rdwrite(),rddump(),rdioctl(),rdsize();
#else
#define	rdopen		enxio
#define	rdstrategy	enxio
#define	rdread		enxio
#define	rdwrite		enxio
#define	rddump		enxio
#define	rdioctl		enxio
#define	rdsize		0
#endif
 
#include "sd.h"
#if NSD > 0
int	sdopen(),sdstrategy(),sdread(),sdwrite(),sddump(),sdioctl(),sdsize();
#else
#define	sdopen		enxio
#define	sdstrategy	enxio
#define	sdread		enxio
#define	sdwrite		enxio
#define	sddump		enxio
#define	sdioctl		enxio
#define	sdsize		0
#endif
 
#include "cd.h"
#if NCD > 0
int	cdopen(),cdstrategy(),cdread(),cdwrite(),cddump(),cdioctl(),cdsize();
#else
#define	cdopen		enxio
#define	cdstrategy	enxio
#define	cdread		enxio
#define	cdwrite		enxio
#define	cddump		enxio
#define	cdioctl		enxio
#define	cdsize		0
#endif
 
#include "vn.h"
#if NVN > 0
int	vnopen(),vnstrategy(),vnread(),vnwrite(),vndump(),vnioctl(),vnsize();
#else
#define	vnopen		enxio
#define	vnstrategy	enxio
#define	vnread		enxio
#define	vnwrite		enxio
#define	vndump		enxio
#define	vnioctl		enxio
#define	vnsize		0
#endif
 
struct bdevsw	bdevsw[] =
{
	{ ctopen,	ctclose,	ctstrategy,	ctioctl,	/*0*/
	  ctdump,	0,		B_TAPE },
	{ enxio,	enxio,		enxio,		enxio,		/*1*/
	  enxio,	0,		0 },
	{ rdopen,	nullop,		rdstrategy,	rdioctl,	/*2*/
	  rddump,	rdsize,		0 },
	{ enodev,	enodev,		swstrategy,	enodev,		/*3*/
	  enodev,	0,		0 },
	{ sdopen,	nullop,		sdstrategy,	sdioctl,	/*4*/
	  sddump,	sdsize,		0 },
	{ cdopen,	nullop,		cdstrategy,	cdioctl,	/*5*/
	  cddump,	cdsize,		0 },
	{ vnopen,	nullop,		vnstrategy,	vnioctl,	/*6*/
	  vndump,	vnsize,		0 },
};
int	nblkdev = sizeof (bdevsw) / sizeof (bdevsw[0]);

int	cnopen(), cnclose(), cnread(),  cnwrite(), cnioctl(), cnselect();

int	cttyopen(), cttyread(), cttywrite(), cttyioctl(), cttyselect();

int 	mmrw();
#define	mmselect	seltrue

#include "pty.h"
#if NPTY > 0
int	ptsopen(),ptsclose(),ptsread(),ptswrite(),ptsstop();
int	ptcopen(),ptcclose(),ptcread(),ptcwrite(),ptcselect();
int	ptyioctl();
struct	tty pt_tty[];
#else
#define ptsopen		enxio
#define ptsclose	enxio
#define ptsread		enxio
#define ptswrite	enxio
#define ptcopen		enxio
#define ptcclose	enxio
#define ptcread		enxio
#define ptcwrite	enxio
#define ptyioctl	enxio
#define	pt_tty		0
#define	ptcselect	enxio
#define	ptsstop		nullop
#endif

#include "ppi.h"
#if NPPI > 0
int	ppiopen(),ppiclose(),ppiread(),ppiwrite(),ppiioctl();
#else
#define ppiopen		enxio
#define ppiclose	enxio
#define ppiread		enxio
#define ppiwrite	enxio
#define ppiioctl	enxio
#endif

#include "ite.h"
#if NITE > 0
int	iteopen(),iteclose(),iteread(),itewrite(),iteioctl();
struct	tty ite_tty[];
#else
#define iteopen		enxio
#define iteclose	enxio
#define iteread		enxio
#define itewrite	enxio
#define itestop		nullop
#define iteioctl	enxio
#define ite_tty		0
#endif

#include "dca.h"
#if NDCA > 0
int	dcaopen(),dcaclose(),dcaread(),dcawrite(),dcastop(),dcaioctl();
struct	tty dca_tty[];
#else
#define dcaopen		enxio
#define dcaclose	enxio
#define dcaread		enxio
#define dcawrite	enxio
#define dcastop		nullop
#define dcaioctl	enxio
#define dca_tty		0
#endif

#include "dcm.h"
#if NDCM > 0
int	dcmopen(),dcmclose(),dcmread(),dcmwrite(),dcmstop(),dcmioctl();
struct	tty dcm_tty[];
#else
#define dcmopen		enxio
#define dcmclose	enxio
#define dcmread		enxio
#define dcmwrite	enxio
#define dcmstop		nullop
#define dcmioctl	enxio
#define dcm_tty		0
#endif

#include "clock.h"
#if NCLOCK > 0
int	clockopen(),clockclose(),clockioctl(),clockmap();
#else
#define clockopen	enxio
#define clockclose	enxio
#define clockioctl	enxio
#define clockmap	enxio
#endif

#include "bpfilter.h"
#if NBPFILTER > 0
int	bpfopen(),bpfclose(),bpfread(),bpfwrite(),bpfioctl(),bpfselect();
#else
#define bpfopen		enxio
#define bpfclose	enxio
#define bpfread		enxio
#define bpfwrite	enxio
#define bpfioctl	enxio
#define bpfselect	enxio
#endif

int	logopen(),logclose(),logread(),logioctl(),logselect();

int	fdopen();

int	ttselect(), seltrue();

int	grfopen(),grfclose(),grfioctl(),grfselect(),grfmap();

int	hilopen(),hilclose(),hilioctl(),hilmap(),hilselect(),hilread();

struct cdevsw	cdevsw[] =
{
	{ cnopen,	cnclose,	cnread,		cnwrite,	/*0*/
	  cnioctl,	nullop,		nullop,		NULL,
	  cnselect,	enodev,		NULL },
	{ cttyopen,	nullop,		cttyread,	cttywrite,	/*1*/
	  cttyioctl,	nullop,		nullop,		NULL,
	  cttyselect,	enodev,		NULL },
	{ nullop,	nullop,		mmrw,		mmrw,		/*2*/
	  enodev,	nullop,		nullop,		NULL,
	  mmselect,	enodev,		NULL },
	{ nullop,	nullop,		rawread,	rawwrite,	/*3*/
	  enodev,	enodev,		nullop,		NULL,
	  enodev,	enodev,		swstrategy },
	{ ptsopen,	ptsclose,	ptsread,	ptswrite,	/*4*/
	  ptyioctl,	ptsstop,	nullop,		pt_tty,
	  ttselect,	enodev,		NULL },
	{ ptcopen,	ptcclose,	ptcread,	ptcwrite,	/*5*/
	  ptyioctl,	nullop,		nullop,		pt_tty,
	  ptcselect,	enodev,		NULL },
	{ logopen,	logclose,	logread,	enodev,		/*6*/
	  logioctl,	enodev,		nullop,		NULL,
	  logselect,	enodev,		NULL },
	{ ctopen,	ctclose,	ctread,		ctwrite,	/*7*/
	  ctioctl,	enodev,		nullop,		NULL,
	  seltrue,	enodev,		ctstrategy },
	{ sdopen,	nullop,		sdread,		sdwrite,	/*8*/
	  sdioctl,	enodev,		nullop,		NULL,
	  seltrue,	enodev,		sdstrategy },
	{ rdopen,	nullop,		rdread,		rdwrite,	/*9*/
	  rdioctl,	enodev,		nullop,		NULL,
	  seltrue,	enodev,		rdstrategy },
	{ grfopen,	grfclose,	nullop,		nullop,		/*10*/
	  grfioctl,	enodev,		nullop,		NULL,
	  grfselect,	grfmap,		NULL },
	{ ppiopen,	ppiclose,	ppiread,	ppiwrite,	/*11*/
	  ppiioctl,	enodev,		nullop,		NULL,
	  enodev,	enodev,		NULL },
	{ dcaopen,	dcaclose,	dcaread,	dcawrite,	/*12*/
	  dcaioctl,	dcastop,	nullop,		dca_tty,
	  ttselect,	enodev,		NULL },
	{ iteopen,	iteclose,	iteread,	itewrite,	/*13*/
	  iteioctl,	enodev,		nullop,		ite_tty,
	  ttselect,	enodev,		NULL },
	{ hilopen,	hilclose,	hilread,	nullop,		/*14*/
	  hilioctl,	enodev,		nullop,		NULL,
	  hilselect,	hilmap,		NULL },
	{ dcmopen,	dcmclose,	dcmread,	dcmwrite,	/*15*/
	  dcmioctl,	dcmstop,	nullop,		dcm_tty,
	  ttselect,	enodev,		NULL },
	{ enodev,	enodev,		enodev,		enodev,		/*16*/
	  enodev,	enodev,		nullop,		NULL,
	  seltrue,	enodev,		NULL },
	{ cdopen,	nullop,		cdread,		cdwrite,	/*17*/
	  cdioctl,	enodev,		nullop,		NULL,
	  seltrue,	enodev,		cdstrategy },
	{ clockopen,	clockclose,	nullop,		nullop,		/*18*/
	  clockioctl,	enodev,		nullop,		NULL,
	  nullop,	clockmap,	NULL },
	{ vnopen,	nullop,		vnread,		vnwrite,	/*19*/
	  vnioctl,	enodev,		nullop,		NULL,
	  seltrue,	enodev,		NULL },
	{ enodev,	enodev,		enodev,		enodev,		/*20*/
	  enodev,	enodev,		nullop,		NULL,
	  seltrue,	enodev,		NULL },
	{ fdopen,	enodev,		enodev,		enodev,		/*21*/
	  enodev,	enodev,		enodev,		NULL,
	  enodev,	enodev,		NULL },
	{ bpfopen,	bpfclose,	bpfread,	bpfwrite,	/*22*/
	  bpfioctl,	enodev,		enodev,		NULL,
	  bpfselect,	enodev,		NULL },
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
