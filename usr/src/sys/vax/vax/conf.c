/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)conf.c	7.7 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "buf.h"
#include "ioctl.h"
#include "tty.h"
#include "conf.h"

int	nulldev();
int	nodev();

#include "hp.h"
#if NHP > 0
int	hpopen(),hpclose(),hpstrategy();
int	hpread(),hpwrite(),hpioctl(),hpdump(),hpsize();
#else
#define	hpopen		nodev
#define	hpclose		nodev
#define	hpstrategy	nodev
#define	hpread		nodev
#define	hpwrite		nodev
#define	hpioctl		nodev
#define	hpdump		nodev
#define	hpsize		0
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
int	rkopen(),rkstrategy(),rkread(),rkwrite(),rkintr();
int	rkdump(),rkreset(),rksize();
#else
#define	rkopen		nodev
#define	rkstrategy	nodev
#define	rkread		nodev
#define	rkwrite		nodev
#define	rkintr		nodev
#define	rkdump		nodev
#define	rkreset		nodev
#define	rksize		0
#endif

#include "te.h"
#if NTE > 0
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
#define	tmreset		nulldev
#endif

#include "tms.h"
#if NTMS > 0
int	tmscpopen(),tmscpclose(),tmscpstrategy(),tmscpread(),tmscpwrite();
int	tmscpioctl(),tmscpdump(),tmscpreset();
#else
#define	tmscpopen	nodev
#define	tmscpclose	nodev
#define	tmscpstrategy	nodev
#define	tmscpread	nodev
#define	tmscpwrite	nodev
#define	tmscpioctl	nodev
#define	tmscpdump	nodev
#define	tmscpreset	nulldev
#endif

#include "ts.h"
#if NTS > 0
int	tsopen(),tsclose(),tsstrategy(),tsread(),tswrite();
int	tsioctl(),tsdump(),tsreset();
#else
#define	tsopen		nodev
#define	tsclose		nodev
#define	tsstrategy	nodev
#define	tsread		nodev
#define	tswrite		nodev
#define	tsioctl		nodev
#define	tsdump		nodev
#define	tsreset		nulldev
#endif

#include "mu.h"
#if NMT > 0
int	mtopen(),mtclose(),mtstrategy(),mtread(),mtwrite();
int	mtioctl(),mtdump();
#else
#define	mtopen		nodev
#define	mtclose		nodev
#define	mtstrategy	nodev
#define	mtread		nodev
#define	mtwrite		nodev
#define	mtioctl		nodev
#define	mtdump		nodev
#endif

#include "ra.h"
#if NUDA > 0
int	udopen(),udclose(),udstrategy(),udread(),udwrite(),udioctl();
int	udreset(),uddump(),udsize();
#else
#define	udopen		nodev
#define	udclose		nodev
#define	udstrategy	nodev
#define	udread		nodev
#define	udwrite		nodev
#define	udioctl		nodev
#define	udreset		nulldev
#define	uddump		nodev
#define	udsize		0
#endif

#include "up.h"
#if NSC > 0
int	upopen(),upstrategy(),upread(),upwrite(),upreset(),updump(),upsize();
#else
#define	upopen		nodev
#define	upstrategy	nodev
#define	upread		nodev
#define	upwrite		nodev
#define	upreset		nulldev
#define	updump		nodev
#define	upsize		0
#endif

#include "tj.h"
#if NUT > 0
int	utopen(),utclose(),utstrategy(),utread(),utwrite(),utioctl();
int	utreset(),utdump();
#else
#define	utopen		nodev
#define	utclose		nodev
#define	utread		nodev
#define	utstrategy	nodev
#define	utwrite		nodev
#define	utreset		nulldev
#define	utioctl		nodev
#define	utdump		nodev
#endif

#include "rb.h"
#if NIDC > 0
int	idcopen(),idcstrategy(),idcread(),idcwrite();
int	idcreset(),idcdump(),idcsize();;
#else
#define	idcopen		nodev
#define	idcstrategy	nodev
#define	idcread		nodev
#define	idcwrite	nodev
#define	idcreset	nulldev
#define	idcdump		nodev
#define	idcsize		0
#endif

#if defined(VAX750) || defined(VAX730)
int	tuopen(),tuclose(),tustrategy();
#else
#define	tuopen		nodev
#define	tuclose		nodev
#define	tustrategy	nodev
#endif

#include "rx.h"
#if NFX > 0
int	rxopen(),rxstrategy(),rxclose(),rxread(),rxwrite(),rxreset(),rxioctl();
#else
#define	rxopen		nodev
#define rxstrategy	nodev
#define	rxclose		nodev
#define	rxread		nodev
#define	rxwrite		nodev
#define	rxreset		nulldev
#define	rxioctl		nodev
#endif

#include "uu.h"
#if NUU > 0
int	uuopen(),uustrategy(),uuclose(),uureset(),uuioctl();
#else
#define	uuopen		nodev
#define uustrategy	nodev
#define	uuclose		nodev
#define	uureset		nulldev
#define	uuioctl		nodev
#endif

#include "rl.h"
#if NRL > 0
int	rlopen(),rlstrategy(),rlread(),rlwrite();
int	rlreset(),rldump(),rlsize();
#else
#define	rlopen		nodev
#define	rlstrategy	nodev
#define	rlread		nodev
#define	rlwrite		nodev
#define	rlreset		nulldev
#define	rldump		nodev
#define	rlsize		0
#endif

#include "np.h"
#if NNP > 0
int	npopen(),npclose(),npread(),npwrite();
int	npreset(),npioctl();
#else
#define	npopen		nodev
#define	npclose		nodev
#define	npread		nodev
#define	npwrite		nodev
#define	npreset		nulldev
#define	npioctl		nodev
#endif

int	swstrategy(),swread(),swwrite();

struct bdevsw	bdevsw[] =
{
	{ hpopen,	hpclose,	hpstrategy,	hpioctl,	/*0*/
	  hpdump,	hpsize,		0 },
	{ htopen,	htclose,	htstrategy,	htioctl,	/*1*/
	  htdump,	0,		B_TAPE },
	{ upopen,	nulldev,	upstrategy,	nodev,		/*2*/
	  updump,	upsize,		0 },
	{ rkopen,	nulldev,	rkstrategy,	nodev,		/*3*/
	  rkdump,	rksize,		0 },
	{ nodev,	nodev,		swstrategy,	nodev,		/*4*/
	  nodev,	0,		0 },
	{ tmopen,	tmclose,	tmstrategy,	tmioctl,	/*5*/
	  tmdump,	0,		B_TAPE },
	{ tsopen,	tsclose,	tsstrategy,	tsioctl,	/*6*/
	  tsdump,	0,		B_TAPE },
	{ mtopen,	mtclose,	mtstrategy,	mtioctl,	/*7*/
	  mtdump,	0,		B_TAPE },
	{ tuopen,	tuclose,	tustrategy,	nodev,		/*8*/
	  nodev,	0,		B_TAPE },
	{ udopen,	udclose,	udstrategy,	udioctl,	/*9*/
	  uddump,	udsize,		0 },
	{ utopen,	utclose,	utstrategy,	utioctl,	/*10*/
	  utdump,	0,		B_TAPE },
	{ idcopen,	nodev,		idcstrategy,	nodev,		/*11*/
	  idcdump,	idcsize,	0 },
	{ rxopen,	rxclose,	rxstrategy,	nodev,		/*12*/
	  nodev,	0,		0 },
	{ uuopen,	uuclose,	uustrategy,	nodev,		/*13*/
	  nodev,	0,		0 },
	{ rlopen,	nodev,		rlstrategy,	nodev,		/*14*/
	  rldump,	rlsize,		0 },
	{ tmscpopen,	tmscpclose,	tmscpstrategy,	tmscpioctl,	/*15*/
	  tmscpdump,	0,		B_TAPE },
};
int	nblkdev = sizeof (bdevsw) / sizeof (bdevsw[0]);

int	cnopen(),cnclose(),cnread(),cnwrite(),cnioctl();
struct tty cons;

#include "acc.h"
#if NACC > 0
int     accreset();
#else
#define accreset nulldev
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

#include "dmf.h"
#if NDMF == 0
#define	dmfopen	nodev
#define	dmfclose	nodev
#define	dmfread	nodev
#define	dmfwrite	nodev
#define	dmfioctl	nodev
#define	dmfstop	nodev
#define	dmfreset	nulldev
#define	dmf_tty	0
#else
int	dmfopen(),dmfclose(),dmfread(),dmfwrite(),dmfioctl(),dmfstop(),dmfreset();
struct	tty dmf_tty[];
#endif

#if VAX8600
int	crlopen(),crlclose(),crlread(),crlwrite();
#else
#define	crlopen		nodev
#define	crlclose	nodev
#define	crlread		nodev
#define	crlwrite	nodev
#endif

#if VAX780
int	flopen(),flclose(),flread(),flwrite();
#else
#define	flopen	nodev
#define	flclose	nodev
#define	flread	nodev
#define	flwrite	nodev
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

int	syopen(),syread(),sywrite(),syioctl(),syselect();

int 	mmread(),mmwrite();
#define	mmselect	seltrue

#include "va.h"
#if NVA > 0
int	vaopen(),vaclose(),vawrite(),vaioctl(),vareset(),vaselect();
#else
#define	vaopen		nodev
#define	vaclose		nodev
#define	vawrite		nodev
#define	vaopen		nodev
#define	vaioctl		nodev
#define	vareset		nulldev
#define	vaselect	nodev
#endif

#include "vp.h"
#if NVP > 0
int	vpopen(),vpclose(),vpwrite(),vpioctl(),vpreset(),vpselect();
#else
#define	vpopen		nodev
#define	vpclose		nodev
#define	vpwrite		nodev
#define	vpioctl		nodev
#define	vpreset		nulldev
#define	vpselect	nodev
#endif

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

#include "lpa.h"
#if NLPA > 0
int	lpaopen(),lpaclose(),lparead(),lpawrite(),lpaioctl();
#else
#define	lpaopen		nodev
#define	lpaclose	nodev
#define	lparead		nodev
#define	lpawrite	nodev
#define	lpaioctl	nodev
#endif

#include "dn.h"
#if NDN > 0
int	dnopen(),dnclose(),dnwrite();
#else
#define	dnopen		nodev
#define	dnclose		nodev
#define	dnwrite		nodev
#endif

#include "ik.h"
#if NIK > 0
int	ikopen(),ikclose(),ikread(),ikwrite(),ikioctl(),ikreset();
#else
#define ikopen nodev
#define ikclose nodev
#define ikread nodev
#define ikwrite nodev
#define ikioctl nodev
#define ikreset nulldev
#endif

#include "ps.h"
#if NPS > 0
int	psopen(),psclose(),psread(),pswrite(),psioctl(),psreset();
#else
#define psopen nodev
#define psclose nodev
#define psread nodev
#define pswrite nodev
#define psopen nodev
#define psioctl nodev
#define psreset nulldev
#endif

#include "ad.h"
#if NAD > 0
int	adopen(),adclose(),adioctl(),adreset();
#else
#define adopen nodev
#define adclose nodev
#define adioctl nodev
#define adreset nulldev
#endif

int	logopen(),logclose(),logread(),logioctl(),logselect();

#include "dhu.h"
#if NDHU > 0
int dhuopen(),dhuclose(),dhuread(),dhuwrite(),dhuioctl(),dhustop(),dhureset();
struct tty dhu_tty[];
#else
#define dhuopen nodev
#define dhuclose nodev
#define dhuread nodev
#define dhuwrite nodev
#define dhuioctl nodev
#define dhustop nodev
#define dhureset nulldev
#define dhu_tty 0
#endif

#include "vs.h"
#if NVS > 0
int	vsopen(),vsclose(),vsioctl(),vsreset(),vsselect();
#else
#define vsopen nodev
#define vsclose nodev
#define vsioctl nodev
#define vsreset nodev
#define vsselect nodev
#endif

#include "dmz.h"
#if NDMZ > 0
int dmzopen(),dmzclose(),dmzread(),dmzwrite(),dmzioctl(),dmzstop(),dmzreset();
struct tty dmz_tty[];
#else
#define dmzopen nodev
#define dmzclose nodev
#define dmzread nodev
#define dmzwrite nodev
#define dmzioctl nodev
#define dmzstop nodev
#define dmzreset nulldev
#define dmz_tty 0
#endif

#include "qv.h"
#if NQV > 0
int	qvopen(), qvclose(), qvread(), qvwrite(), qvioctl(), qvstop(),
	qvreset(), qvselect(), qvcons_init();
#else
#define qvopen	nodev
#define qvclose	nodev
#define qvread	nodev
#define qvwrite	nodev
#define qvioctl	nodev
#define qvstop	nodev
#define qvreset	nulldev
#define qvselect	nodev
#define qvcons_init	nodev
#endif

#if defined(INGRES)
int	iiioctl(), iiclose(), iiopen();
#else
#define iiopen nodev
#define iiclose nodev
#define iiioctl nodev
#endif

int	ttselect(), seltrue();

struct cdevsw	cdevsw[] =
{
	cnopen,		cnclose,	cnread,		cnwrite,	/*0*/
	cnioctl,	nulldev,	nulldev,	&cons,
	ttselect,	nodev,
	dzopen,		dzclose,	dzread,		dzwrite,	/*1*/
	dzioctl,	dzstop,		dzreset,	dz_tty,
	ttselect,	nodev,
	syopen,		nulldev,	syread,		sywrite,	/*2*/
	syioctl,	nulldev,	nulldev,	0,
	syselect,	nodev,
	nulldev,	nulldev,	mmread,		mmwrite,	/*3*/
	nodev,		nulldev,	nulldev,	0,
	mmselect,	nodev,
	hpopen,		hpclose,	hpread,		hpwrite,	/*4*/
	hpioctl,	nodev,		nulldev,	0,
	seltrue,	nodev,
	htopen,		htclose,	htread,		htwrite,	/*5*/
	htioctl,	nodev,		nulldev,	0,
	seltrue,	nodev,
	vpopen,		vpclose,	nodev,		vpwrite,	/*6*/
	vpioctl,	nulldev,	vpreset,	0,
	vpselect,	nodev,
	nulldev,	nulldev,	swread,		swwrite,	/*7*/
	nodev,		nodev,		nulldev,	0,
	nodev,		nodev,
	flopen,		flclose,	flread,		flwrite,	/*8*/
	nodev,		nodev,		nulldev,	0,
	seltrue,	nodev,
	udopen,		udclose,	udread,		udwrite,	/*9*/
	udioctl,	nodev,		udreset,	0,
	seltrue,	nodev,
	vaopen,		vaclose,	nodev,		vawrite,	/*10*/
	vaioctl,	nulldev,	vareset,	0,
	vaselect,	nodev,
	rkopen,		nulldev,	rkread,		rkwrite,	/*11*/
	nodev,		nodev,		rkreset,	0,
	seltrue,	nodev,
	dhopen,		dhclose,	dhread,		dhwrite,	/*12*/
	dhioctl,	dhstop,		dhreset,	dh11,
	ttselect,	nodev,
	upopen,		nulldev,	upread,		upwrite,	/*13*/
	nodev,		nodev,		upreset,	0,
	seltrue,	nodev,
	tmopen,		tmclose,	tmread,		tmwrite,	/*14*/
	tmioctl,	nodev,		tmreset,	0,
	seltrue,	nodev,
	lpopen,		lpclose,	nodev,		lpwrite,	/*15*/
	nodev,		nodev,		lpreset,	0,
	seltrue,	nodev,
	tsopen,		tsclose,	tsread,		tswrite,	/*16*/
	tsioctl,	nodev,		tsreset,	0,
	seltrue,	nodev,
	utopen,		utclose,	utread,		utwrite,	/*17*/
	utioctl,	nodev,		utreset,	0,
	seltrue,	nodev,
	ctopen,		ctclose,	nodev,		ctwrite,	/*18*/
	nodev,		nodev,		nulldev,	0,
	seltrue,	nodev,
	mtopen,		mtclose,	mtread,		mtwrite,	/*19*/
	mtioctl,	nodev,		nodev,		0,
	seltrue,	nodev,
	ptsopen,	ptsclose,	ptsread,	ptswrite,	/*20*/
	ptyioctl,	ptsstop,	nulldev,	pt_tty,
	ttselect,	nodev,
	ptcopen,	ptcclose,	ptcread,	ptcwrite,	/*21*/
	ptyioctl,	nulldev,	nulldev,	pt_tty,
	ptcselect,	nodev,
	dmfopen,	dmfclose,	dmfread,	dmfwrite,	/*22*/
	dmfioctl,	dmfstop,	dmfreset,	dmf_tty,
	ttselect,	nodev,
	idcopen,	nulldev,	idcread,	idcwrite,	/*23*/
	nodev,		nodev,		idcreset,	0,
	seltrue,	nodev,
	dnopen,		dnclose,	nodev,		dnwrite,	/*24*/
	nodev,		nodev,		nulldev,	0,
	seltrue,	nodev,
/* 25-29 reserved to local sites */
	nodev,		nodev,		nodev,		nodev,		/*25*/
	nodev,		nulldev,	nulldev,	0,
	nodev,		nodev,
	lpaopen,	lpaclose,	lparead,	lpawrite,	/*26*/
	lpaioctl,	nodev,		nulldev,	0,
	seltrue,	nodev,
	psopen,		psclose,	psread,		pswrite,	/*27*/
	psioctl,	nodev,		psreset,	0,
	seltrue,	nodev,
	nodev,		nodev,		nodev,		nodev,		/*28*/
	nodev,		nulldev,	nulldev,	0,
	nodev,		nodev,
	adopen,		adclose,	nodev,		nodev,		/*29*/
	adioctl,	nodev,		adreset,	0,
	seltrue,	nodev,
	rxopen,		rxclose,	rxread,		rxwrite,	/*30*/
	rxioctl,	nodev,		rxreset,	0,
	seltrue,	nodev,
	ikopen,		ikclose,	ikread,		ikwrite,	/*31*/
	ikioctl,	nodev,		ikreset,	0,
	seltrue,	nodev,
	rlopen,		nodev,		rlread,		rlwrite,	/*32*/
	nodev,		nodev,		rlreset,	0,
	seltrue,	nodev,
	logopen,	logclose,	logread,	nodev,		/*33*/
	logioctl,	nodev,		nulldev,	0,
	logselect,	nodev,
	dhuopen,        dhuclose,       dhuread,        dhuwrite,       /*34*/
	dhuioctl,       dhustop,        dhureset,       dhu_tty,
	ttselect,       nodev,
 	crlopen,	crlclose,	crlread,	crlwrite,	/*35*/
 	nodev,		nodev,		nulldev,	0,
 	seltrue,	nodev,
	vsopen,		vsclose,	nodev,		nodev,		/*36*/
	vsioctl,	nodev,		vsreset,	0,
	vsselect,	nodev,
	dmzopen,        dmzclose,       dmzread,        dmzwrite,       /*37*/
	dmzioctl,       dmzstop,        dmzreset,       dmz_tty,
	ttselect,       nodev,
	tmscpopen,	tmscpclose,	tmscpread,	tmscpwrite,	/*38*/
	tmscpioctl,	nodev,		tmscpreset,	0,
	seltrue,	nodev,
	npopen,		npclose,	npread,		npwrite,	/*39*/
	npioctl,	nodev,		npreset,	0,
	seltrue,	nodev,
	qvopen,		qvclose,	qvread, 	qvwrite,	/*40*/
	qvioctl,	qvstop,		qvreset,	0,
	qvselect,	nodev,
	nodev,		nodev,		nodev,		nodev,		/*41*/
	nodev,		nulldev,	nulldev,	0,
	nodev,		nodev,
/* 42-50 reserved to local sites */
	nodev,		nodev,		nodev,		nodev,		/*42*/
	nodev,		nulldev,	nulldev,	0,
	nodev,		nodev,
	iiopen,		iiclose,	nulldev,	nulldev,	/*43*/
	iiioctl,	nulldev,	nulldev,	0,
	seltrue,	nodev,
};
int	nchrdev = sizeof (cdevsw) / sizeof (cdevsw[0]);

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
