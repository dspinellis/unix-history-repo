/*	conf.c	4.4	11/12/80	*/

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

#include "../conf/hp.h"
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
 
#include "../conf/ht.h"
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

#include "../conf/up.h"
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
/* 3 reserved for rk07 */
	nodev,		nodev,		nodev,		0,		/*3*/
	nodev,		nodev,		swstrategy,	0,		/*4*/
/* 5 reserved for tm03 */
	0,
};

int	cnopen(),cnclose(),cnread(),cnwrite(),cnioctl();

#include "../conf/dh.h"
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

int	flopen(),flclose(),flread(),flwrite();

#include "../conf/dz.h"
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

int	syopen(),syread(),sywrite(),syioctl();

int 	mmread(),mmwrite();

#include "../conf/va.h"
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

#include "../conf/vp.h"
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

struct cdevsw	cdevsw[] =
{
	cnopen,		cnclose,	cnread,		cnwrite,	/*0*/
	cnioctl,	nulldev,	nulldev,	0,
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
	flopen,		flclose,	flread,		flwrite,	/*8*/
	nodev,		nodev,		nulldev,	0,
	mxopen,		mxclose,	mxread,		mxwrite,	/*9*/
	mxioctl,	nulldev,	nulldev,	0,
	vaopen,		vaclose,	nodev,		vawrite,	/*10*/
	vaioctl,	nulldev,	vareset,	0,
/* 11 reserved for rk07 */
	nodev,		nodev,		nodev,		nodev,		/*11*/
	nodev,		nodev,		nulldev,	0,
	dhopen,		dhclose,	dhread,		dhwrite,	/*12*/
	dhioctl,	dhstop,		dhreset,	dh11,
	nulldev,	nulldev,	upread,		upwrite,	/*13*/
	nodev,		nodev,		upreset,	0,
/* 14 reserved for tm03 */
/* 15 reserved for lp11 */
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
	ttyinput, ttyrend, nulldev, nulldev, nulldev,	/* 0 */
	bkopen, bkclose, bkread, ttwrite, bkioctl,
	bkinput, nodev, nulldev, ttstart, nulldev,	/* 1 */
	ntyopen, ntyclose, ntread, ntwrite, nullioctl,
	ntyinput, ntyrend, nulldev, ttstart, nulldev,	/* 2 */
	mxopen, mxclose, mcread, mcwrite, mxioctl,
	nulldev, nulldev, nulldev, nulldev, nulldev,	/* 3 */
	0
};
 
int	nldisp = 3;
 
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
