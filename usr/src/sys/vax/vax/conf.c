/*	conf.c	4.10	%G%	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/buf.h"
#include "../h/tty.h"
#include "../h/conf.h"
#include "../h/text.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/file.h"
#include "../h/inode.h"
#include "../h/acct.h"
#include "../h/pte.h"
#include "../h/mba.h"

int	nulldev();
int	nodev();

#include "hp.h"
#if NHP > 0
int	hpstrategy(),hpread(),hpwrite(),hpintr();
struct	buf	hptab;
#define	HPTAB	&hptab
#else
#define	hpstrategy	nodev
#define	hpread		nodev
#define	hpwrite		nodev
#define	hpintr		nodev
#define	HPTAB		0
#endif
 
#include "ht.h"
#if NHT > 0
int	htopen(),htclose(),htstrategy(),htread(),htwrite();
struct	buf	httab;
#define	HTTAB	&httab
#else
#define	htopen		nodev
#define	htclose		nodev
#define	htstrategy	nodev
#define	htread		nodev
#define	htwrite		nodev
#define	HTTAB		0
#endif

#include "rk.h"
#if NRK > 0
int	rkstrategy(),rkread(),rkwrite(),rkintr();
struct	buf	rktab;
#define	RKTAB	&rktab
#else
#define	rkstrategy	nodev
#define	rkread		nodev
#define	rkwrite		nodev
#define	rkintr		nodev
#define	RKTAB		0
#endif

#include "tm.h"
#if NTM > 0
int	tmopen(),tmclose(),tmstrategy(),tmread(),tmwrite();
struct	buf	tmtab;
#define	TMTAB	&tmtab
#else
#define	tmopen		nodev
#define	tmclose		nodev
#define	tmstrategy	nodev
#define	tmread		nodev
#define	tmwrite		nodev
#define	TMTAB		0
#endif

#include "ts.h"
#if NTS > 0
int	tsopen(),tsclose(),tsstrategy(),tsread(),tswrite();
struct	buf	tstab;
#define	TSTAB	&tstab
#else
#define	tsopen		nodev
#define	tsclose		nodev
#define	tsstrategy	nodev
#define	tsread		nodev
#define	tswrite		nodev
#define	TSTAB		0
#endif

#include "lp.h"
#if NLP > 0
int	lpopen(),lpclose(),lpwrite(),lpioctl(),lpreset();
#else
#define	lpopen		nodev
#define	lpclose		nodev
#define	lpwrite		nodev
#define	lpioctl		nodev
#define	lpreset		nulldev
#endif

#include "up.h"
#if NUP > 0
int	upstrategy(),upread(),upwrite(),upreset();
struct	buf	uptab;
#define	UPTAB	&uptab
#else
#define	upstrategy	nodev
#define	upread		nodev
#define	upwrite		nodev
#define	upreset		nulldev
#define	UPTAB		0
#endif

int	swstrategy(),swread(),swwrite();

struct bdevsw	bdevsw[] =
{
	nulldev,	nulldev,	hpstrategy,	HPTAB,		/*0*/
	htopen,		htclose,	htstrategy,	HTTAB,		/*1*/
	nulldev,	nulldev,	upstrategy,	UPTAB,		/*2*/
	nulldev,	nulldev,	rkstrategy,	RKTAB,		/*3*/
	nodev,		nodev,		swstrategy,	0,		/*4*/
	tmopen,		tmclose,	tmstrategy,	TMTAB,		/*5*/
	tsopen,		tsclose,	tsstrategy,	TSTAB,		/*6*/
	0,
};

int	cnopen(),cnclose(),cnread(),cnwrite(),cnioctl();
struct tty cons;

#ifdef CHAOS
int	chopen(),chclose(),chread(),chwrite(),chioctl(),chreset();
#else
#define	chopen	nodev
#define	chclose	nodev
#define	chread	nodev
#define	chwrite	nodev
#define	chioctl	nodev
#define	chreset	nodev
#endif

#include "ct.h"
#if NCT > 0
int	ctopen(),ctclose(),ctwrite();
#else
#define	ctopen	nulldev
#define	ctclose	nulldev
#define	ctwrite	nulldev
#endif

#include "dh.h"
#if NDH11 == 0
#define	dhopen	nodev
#define	dhclose	nodev
#define	dhread	nodev
#define	dhwrite	nodev
#define	dhioctl	nodev
#define	dhstop	nodev
#define	dhreset	nulldev
#define	dh11	0
#else
int	dhopen(),dhclose(),dhread(),dhwrite(),dhioctl(),dhstop(),dhreset();
struct	tty dh11[];
#endif

#if VAX==780
int	flopen(),flclose(),flread(),flwrite();
#endif

#include "dk.h"
#if NDK == 0
#define	dkopen	nodev
#define	dkclose	nodev
#define	dkread	nodev
#define	dkwrite	nodev
#define	dkioctl	nodev
#define	dkstop	nodev
#define	dkreset	nulldev
#define	dkchans	0
#else
int	dkopen(),dkclose(),dkread(),dkwrite(),dkioctl(),dkstop(),dkreset();
struct	tty	dkchans[];
#endif

#include "dz.h"
#if NDZ11 == 0
#define	dzopen	nodev
#define	dzclose	nodev
#define	dzread	nodev
#define	dzwrite	nodev
#define	dzioctl	nodev
#define	dzstop	nodev
#define	dzreset	nulldev
#define	dz_tty	0
#else
int	dzopen(),dzclose(),dzread(),dzwrite(),dzioctl(),dzstop(),dzreset();
struct	tty dz_tty[];
#endif

#include "pk.h"
#if NPK == 0
#define	pkopen	nodev
#define	pkclose	nodev
#define	pkread	nodev
#define	pkwrite	nodev
#define	pkioctl	nodev
#define	pkrint	nodev
#define	pkrend	nodev
#define	pkxint	nodev
#define	pkmodem	nodev
#else
int	pkopen(),pkclose(),pkread(),pkioctl();
int	pkrint(),pkrend(),pkxint(),pkmodem();
char	*pkwrite();
#endif

#include "tr.h"
#if HAVTR==0
#define	tropen	nodev
#define	trclose	nodev
#define	trread	nodev
#define	trwrite	nodev
#define	trioctl	nodev
#define	trinput	nodev
#define	trrend	nodev
#define	trmeta	nodev
#define	trxint	nodev
#define	trmodem	nodev
#else
int	tropen(), trclose(), trread(), trioctl(), trinput();
int	trrend(), trmeta(), trxint(), trmodem();
char	*trwrite();
#endif

#include "tdk.h"
#if NTDK == 0
#define	dt_lopen	nodev
#define	dt_lclose	nodev
#define	dt_lread	nodev
#define	dt_lwrite	nodev
#define	dtlinput	nodev
#define	dt_lrend	nodev
#define	dt_lmeta	nodev
#define	dt_lstart	nodev
#else
int	dt_lopen(), dt_lclose(), dt_lread(), dtlinput();
int	dt_lrend(), dt_lmeta(), dt_lstart();
char	*dt_lwrite();
#endif

int	syopen(),syread(),sywrite(),syioctl();

int 	mmread(),mmwrite();

#include "va.h"
#if NVA > 0
int	vaopen(),vaclose(),vawrite(),vaioctl(),vareset();
#else
#define	vaopen	nodev
#define	vaclose	nodev
#define	vawrite	nodev
#define	vaopen	nodev
#define	vaioctl	nodev
#define	vareset	nulldev
#endif

#include "vp.h"
#if NVP > 0
int	vpopen(),vpclose(),vpwrite(),vpioctl(),vpreset();
#else
#define	vpopen	nodev
#define	vpclose	nodev
#define	vpwrite	nodev
#define	vpioctl	nodev
#define	vpreset	nulldev
#endif

int	mxopen(),mxclose(),mxread(),mxwrite(),mxioctl();
int	mcread();
char	*mcwrite();

#if WANTPTY > 0
int	ptsopen(), ptsclose(), ptsread(), ptswrite();
int	ptcopen(), ptcclose(), ptcread(), ptcwrite();
int	ptyioctl();
#else
#define ptsopen nodev
#define ptsclose nodev
#define ptsread nodev
#define ptswrite nodev
#define ptcopen nodev
#define ptcclose nodev
#define ptcread nodev
#define ptcwrite nodev
#define ptyioctl nodev
#endif

struct cdevsw	cdevsw[] =
{
	cnopen,		cnclose,	cnread,		cnwrite,	/*0*/
	cnioctl,	nulldev,	nulldev,	&cons,
	dzopen,		dzclose,	dzread,		dzwrite,	/*1*/
	dzioctl,	dzstop,		dzreset,	dz_tty,
	syopen,		nulldev,	syread,		sywrite,	/*2*/
	syioctl,	nulldev,	nulldev,	0,
	nulldev,	nulldev,	mmread,		mmwrite,	/*3*/
	nodev,		nulldev,	nulldev,	0,
	nulldev,	nulldev,	hpread,		hpwrite,	/*4*/
	nodev,		nodev,		nulldev,	0,
	htopen,		htclose,	htread,		htwrite,	/*5*/
	nodev,		nodev,		nulldev,	0,
	vpopen,		vpclose,	nodev,		vpwrite,	/*6*/
	vpioctl,	nulldev,	vpreset,	0,
	nulldev,	nulldev,	swread,		swwrite,	/*7*/
	nodev,		nodev,		nulldev,	0,
#if VAX==780
	flopen,		flclose,	flread,		flwrite,	/*8*/
	nodev,		nodev,		nulldev,	0,
#else
	nodev,		nodev,		nodev,		nodev,		/*8*/
	nodev,		nodev,		nodev,		0,
#endif
	mxopen,		mxclose,	mxread,		mxwrite,	/*9*/
	mxioctl,	nulldev,	nulldev,	0,
	vaopen,		vaclose,	nodev,		vawrite,	/*10*/
	vaioctl,	nulldev,	vareset,	0,
	nulldev,	nulldev,	rkread,		rkwrite,	/*11*/
	nodev,		nodev,		nulldev,	0,
	dhopen,		dhclose,	dhread,		dhwrite,	/*12*/
	dhioctl,	dhstop,		dhreset,	dh11,
	nulldev,	nulldev,	upread,		upwrite,	/*13*/
	nodev,		nodev,		upreset,	0,
	tmopen,		tmclose,	tmread,		tmwrite,	/*14*/
	nodev,		nodev,		nulldev,	0,
	lpopen,		lpclose,	nodev,		lpwrite,	/*15*/
	lpioctl,	nodev,		lpreset,	0,
	tsopen,		tsclose,	tsread,		tswrite,	/*16*/
	nodev,		nodev,		nulldev,	0,
	dkopen,		dkclose,	dkread,		dkwrite,	/*17*/
	dkioctl,	dkstop,		dkreset,	dkchans,
	ctopen,		ctclose,	nodev,		ctwrite,	/*18*/
	nodev,		nodev,		nodev,		0,
	chopen,		chclose,	chread,		chwrite,	/*19*/
	chioctl,	nulldev,	chreset,	0,
#define CHAOSDEV 19
	ptsopen,	ptsclose,	ptsread,	ptswrite,	/*20*/
	ptyioctl,	nodev,		nodev,		0,
	ptcopen,	ptcclose,	ptcread,	ptcwrite,	/*21*/
	ptyioctl,	nodev,		nodev,		0,
	nodev,		nodev,		nodev,		nodev,		/*22*/
	nodev,		nodev,		nodev,		0,
/* 25-29 reserved to local sites */
	0,	
};

int	ttyopen(),ttread(),nullioctl(),ttstart();
char	*ttwrite();
int	ttyinput(),ttyrend();
int	bkopen(),bkclose(),bkread(),bkinput(),bkioctl();
int	ntyopen(),ntyclose(),ntread();
char	*ntwrite();
int	ntyinput(),ntyrend();
 
struct	linesw linesw[] =
{
	ttyopen, nulldev, ttread, ttwrite, nullioctl,
	ttyinput, ttyrend, nulldev, nulldev, nulldev,		/* 0 */
	bkopen, bkclose, bkread, ttwrite, bkioctl,
	bkinput, nodev, nulldev, ttstart, nulldev,		/* 1 */
	ntyopen, ntyclose, ntread, ntwrite, nullioctl,
	ntyinput, ntyrend, nulldev, ttstart, nulldev,		/* 2 */
	pkopen, pkclose, pkread, pkwrite, pkioctl,	
	pkrint, pkrend, nulldev, pkxint, pkmodem,		/* 3 */
	tropen, trclose, trread, trwrite, trioctl,
	trinput, trrend, trmeta, trxint, trmodem,		/* 4 */
	dt_lopen, dt_lclose, dt_lread, dt_lwrite, nullioctl,
	dtlinput, dt_lrend, dt_lmeta, dt_lstart, nulldev, 	/* 5 */
	mxopen, mxclose, mcread, mcwrite, mxioctl,
	nulldev, nulldev, nulldev, nulldev, nulldev,		/* 6 */
	0
};
 
int	nldisp = 6;
 
struct	buf	buf[NBUF];
struct	file	file[NFILE];
struct	inode	inode[NINODE];
struct	text	text[NTEXT];
struct	proc	proc[NPROC];
struct	buf	bfreelist;
struct	buf	bswlist;	/* free list of swap headers */
struct	buf	*bclnlist;	/* header for list of cleaned pages */
struct	acct	acctbuf;
struct	inode	*acctp;
 
int	mem_no = 3; 	/* major device number of memory special file */

/*
 * Swapdev is a fake device implemented
 * in sw.c used only internally to get to swstrategy.
 * It cannot be provided to the users, because the
 * swstrategy routine munches the b_dev and b_blkno entries
 * before calling the appropriate driver.  This would horribly
 * confuse, e.g. the hashing routines as well as the placement
 * of the block on the d_tab chains.  Instead, /dev/drum is
 * provided as a character (raw) device.
 */
dev_t	swapdev = makedev(4, 0);

extern struct user u;

#if VAX==780
/*
 * This is stupid, and will go away soon.
 */
int	mbanum[] = {	/* mba number of major device */
	0,		/* disk */
	1,		/* tape */
	9999999,	/* unused */
	9999999,	/* unused */
	0,		/* disk, raw */
	1,		/* tape, raw */
};

extern	struct pte MBA0map[], MBA1map[];

struct	mba_info mbainfo[] = {
	MBA0,	PHYSMBA0,	MBA0map,
	MBA1,	PHYSMBA1,	MBA1map
};
#endif
#ifdef	CHAOS
int cdevpath = 1 << CHAOSDEV;
#endif
