/*	conf.c	4.28	81/03/11	*/

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

int	nulldev();
int	nodev();

#include "hp.h"
#if NHP > 0
int	hpstrategy(),hpread(),hpwrite(),hpintr(),hpdump();
#else
#define	hpstrategy	nodev
#define	hpread		nodev
#define	hpwrite		nodev
#define	hpintr		nodev
#define	hpdump		nodev
#endif
 
#include "tu.h"
#if NHT > 0
int	htopen(),htclose(),htstrategy(),htread(),htwrite(),htdump(),htioctl();
#else
#define	htopen		nodev
#define	htclose		nodev
#define	htstrategy	nodev
#define	htread		nodev
#define	htwrite		nodev
#define	htdump		nodev
#define	htioctl		nodev
#endif

#include "rk.h"
#if NHK > 0
int	rkstrategy(),rkread(),rkwrite(),rkintr(),rkdump(),rkreset();
#else
#define	rkstrategy	nodev
#define	rkread		nodev
#define	rkwrite		nodev
#define	rkintr		nodev
#define	rkdump		nodev
#define	rkreset		nodev
#endif

#include "te.h"
#if NTM > 0
int	tmopen(),tmclose(),tmstrategy(),tmread(),tmwrite();
int	tmioctl(),tmdump(),tmreset();
#else
#define	tmopen		nodev
#define	tmclose		nodev
#define	tmstrategy	nodev
#define	tmread		nodev
#define	tmwrite		nodev
#define	tmioctl		nodev
#define	tmdump		nodev
#define	tmreset		nodev
#endif

#define	tsopen		nodev
#define	tsclose		nodev
#define	tsstrategy	nodev
#define	tsread		nodev
#define	tswrite		nodev
#define	tsdump		nodev

#include "up.h"
#if NSC > 0
int	upstrategy(),upread(),upwrite(),upreset(),updump();
#else
#define	upstrategy	nodev
#define	upread		nodev
#define	upwrite		nodev
#define	upreset		nulldev
#define	updump		nodev
#endif

int	swstrategy(),swread(),swwrite();

struct bdevsw	bdevsw[] =
{
	nulldev,	nulldev,	hpstrategy,	hpdump,	0,	/*0*/
	htopen,		htclose,	htstrategy,	htdump,	B_TAPE,	/*1*/
	nulldev,	nulldev,	upstrategy,	updump,	0,	/*2*/
	nulldev,	nulldev,	rkstrategy,	rkdump,	0,	/*3*/
	nodev,		nodev,		swstrategy,	nodev,	0,	/*4*/
	tmopen,		tmclose,	tmstrategy,	tmdump,	B_TAPE,	/*5*/
	tsopen,		tsclose,	tsstrategy,	tsdump,	B_TAPE,	/*6*/
	0,
};

int	cnopen(),cnclose(),cnread(),cnwrite(),cnioctl();
struct tty cons;

#include "ct.h"
#if NCT > 0
int	ctopen(),ctclose(),ctwrite();
#else
#define	ctopen	nulldev
#define	ctclose	nulldev
#define	ctwrite	nulldev
#endif

#include "dh.h"
#if NDH == 0
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

#if VAX780
int	flopen(),flclose(),flread(),flwrite();
#endif

#include "dz.h"
#if NDZ == 0
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

#include "lp.h"
#if NLP > 0
int	lpopen(),lpclose(),lpwrite(),lpreset();
#else
#define	lpopen		nodev
#define	lpclose		nodev
#define	lpwrite		nodev
#define	lpreset		nulldev
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

#include "pty.h"
#if NPTY > 0
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
	htioctl,	nodev,		nulldev,	0,
	vpopen,		vpclose,	nodev,		vpwrite,	/*6*/
	vpioctl,	nulldev,	vpreset,	0,
	nulldev,	nulldev,	swread,		swwrite,	/*7*/
	nodev,		nodev,		nulldev,	0,
#if VAX780
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
	nodev,		nodev,		rkreset,	0,
	dhopen,		dhclose,	dhread,		dhwrite,	/*12*/
	dhioctl,	dhstop,		dhreset,	dh11,
	nulldev,	nulldev,	upread,		upwrite,	/*13*/
	nodev,		nodev,		upreset,	0,
	tmopen,		tmclose,	tmread,		tmwrite,	/*14*/
	tmioctl,	nodev,		tmreset,	0,
	lpopen,		lpclose,	nodev,		lpwrite,	/*15*/
	nodev,		nodev,		lpreset,	0,
	tsopen,		tsclose,	tsread,		tswrite,	/*16*/
	nodev,		nodev,		nulldev,	0,
	nodev,		nodev,		nodev,		nodev,		/*17*/
	nodev,		nodev,		nulldev,	0,
	ctopen,		ctclose,	nodev,		ctwrite,	/*18*/
	nodev,		nodev,		nulldev,	0,
	nodev,		nodev,		nodev,		nodev,		/*19*/
	nodev,		nodev,		nulldev,	0,
	ptsopen,	ptsclose,	ptsread,	ptswrite,	/*20*/
	ptyioctl,	nodev,		nodev,		0,
	ptcopen,	ptcclose,	ptcread,	ptcwrite,	/*21*/
	ptyioctl,	nodev,		nodev,		0,
	nodev,		nodev,		nodev,		nodev,		/*22*/
	nodev,		nodev,		nodev,		0,
	nodev,		nodev,		nodev,		nodev,		/*23*/
	nodev,		nodev,		nodev,		0,
	nodev,		nodev,		nodev,		nodev,		/*24*/
	nodev,		nodev,		nodev,		0,
/* 25-29 reserved to local sites */
	0,	
};

int	ttyopen(),ttread(),nullioctl(),ttstart();
char	*ttwrite();
int	ttyinput(),ttyrend();

#include "bk.h"
#if NBK > 0
int	bkopen(),bkclose(),bkread(),bkinput(),bkioctl();
#endif

int	ntyopen(),ntyclose(),ntread();
char	*ntwrite();
int	ntyinput(),ntyrend();
 
struct	linesw linesw[] =
{
	ttyopen, nulldev, ttread, ttwrite, nullioctl,
	ttyinput, ttyrend, nulldev, nulldev, nulldev,		/* 0 */
#if NBK > 0
	bkopen, bkclose, bkread, ttwrite, bkioctl,
	bkinput, nodev, nulldev, ttstart, nulldev,		/* 1 */
#else
	nodev, nodev, nodev, nodev, nodev,
	nodev, nodev, nodev, nodev, nodev,
#endif
	ntyopen, ntyclose, ntread, ntwrite, nullioctl,
	ntyinput, ntyrend, nulldev, ttstart, nulldev,		/* 2 */
	mxopen, mxclose, mcread, mcwrite, mxioctl,
	nulldev, nulldev, nulldev, nulldev, nulldev,		/* 3 */
	0
};
 
int	nldisp = 4;
 
struct	buf	bfreelist[BQUEUES];	/* buffer chain headers */
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
 * confuse, e.g. the hashing routines. Instead, /dev/drum is
 * provided as a character (raw) device.
 */
dev_t	swapdev = makedev(4, 0);

extern struct user u;
