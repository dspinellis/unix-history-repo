/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)conf.c	7.15 (Berkeley) 4/10/90
 */

#include "param.h"
#include "systm.h"
#include "buf.h"
#include "ioctl.h"
#include "tty.h"
#include "conf.h"

int nulldev(), nodev(), rawread(), rawwrite(), swstrategy();

#include "hp.h"
#if NHP > 0
int	hpopen(),hpclose(),hpstrategy(),hpioctl(),hpdump(),hpsize();
#else
#define	hpopen		nodev
#define	hpclose		nodev
#define	hpstrategy	nodev
#define	hpioctl		nodev
#define	hpdump		nodev
#define	hpsize		0
#endif
 
#include "tu.h"
#if NHT > 0
int	htopen(),htclose(),htstrategy(),htdump(),htioctl();
#else
#define	htopen		nodev
#define	htclose		nodev
#define	htstrategy	nodev
#define	htdump		nodev
#define	htioctl		nodev
#endif

#include "rk.h"
#if NHK > 0
int	rkopen(),rkstrategy(),rkintr(),rkdump(),rkreset(),rksize();
#else
#define	rkopen		nodev
#define	rkstrategy	nodev
#define	rkintr		nodev
#define	rkdump		nodev
#define	rkreset		nodev
#define	rksize		0
#endif

#include "te.h"
#if NTE > 0
int	tmopen(),tmclose(),tmstrategy(),tmioctl(),tmdump(),tmreset();
#else
#define	tmopen		nodev
#define	tmclose		nodev
#define	tmstrategy	nodev
#define	tmioctl		nodev
#define	tmdump		nodev
#define	tmreset		nulldev
#endif

#include "tms.h"
#if NTMS > 0
int	tmscpopen(),tmscpclose(),tmscpstrategy();
int	tmscpioctl(),tmscpdump(),tmscpreset();
#else
#define	tmscpopen	nodev
#define	tmscpclose	nodev
#define	tmscpstrategy	nodev
#define	tmscpioctl	nodev
#define	tmscpdump	nodev
#define	tmscpreset	nulldev
#endif

#include "ts.h"
#if NTS > 0
int	tsopen(),tsclose(),tsstrategy(),tsioctl(),tsdump(),tsreset();
#else
#define	tsopen		nodev
#define	tsclose		nodev
#define	tsstrategy	nodev
#define	tsioctl		nodev
#define	tsdump		nodev
#define	tsreset		nulldev
#endif

#include "mu.h"
#if NMT > 0
int	mtopen(),mtclose(),mtstrategy(),mtioctl(),mtdump();
#else
#define	mtopen		nodev
#define	mtclose		nodev
#define	mtstrategy	nodev
#define	mtioctl		nodev
#define	mtdump		nodev
#endif

#include "ra.h"
#if NUDA > 0
int	udaopen(),udaclose(),udastrategy();
int	udaioctl(),udareset(),udadump(),udasize();
#else
#define	udaopen		nodev
#define	udaclose	nodev
#define	udastrategy	nodev
#define	udaioctl	nodev
#define	udareset	nulldev
#define	udadump		nodev
#define	udasize		0
#endif

#include "kra.h"
#if NKDB > 0
int	kdbopen(),kdbstrategy(),kdbdump(),kdbsize();
#else
#define	kdbopen		nodev
#define	kdbstrategy	nodev
#define	kdbdump		nodev
#define	kdbsize		0
#endif

#include "up.h"
#if NSC > 0
int	upopen(),upstrategy(),upreset(),updump(),upsize();
#else
#define	upopen		nodev
#define	upstrategy	nodev
#define	upreset		nulldev
#define	updump		nodev
#define	upsize		0
#endif

#include "tj.h"
#if NUT > 0
int	utopen(),utclose(),utstrategy(),utioctl(),utreset(),utdump();
#else
#define	utopen		nodev
#define	utclose		nodev
#define	utstrategy	nodev
#define	utreset		nulldev
#define	utioctl		nodev
#define	utdump		nodev
#endif

#include "rb.h"
#if NIDC > 0
int	idcopen(),idcstrategy(),idcreset(),idcdump(),idcsize();;
#else
#define	idcopen		nodev
#define	idcstrategy	nodev
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
int	rlopen(),rlstrategy(),rlreset(),rldump(),rlsize();
#else
#define	rlopen		nodev
#define	rlstrategy	nodev
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
	{ udaopen,	udaclose,	udastrategy,	udaioctl,	/*9*/
	  udadump,	udasize,	0 },
	{ utopen,	utclose,	utstrategy,	utioctl,	/*10*/
	  utdump,	0,		B_TAPE },
	{ idcopen,	nulldev,	idcstrategy,	nodev,		/*11*/
	  idcdump,	idcsize,	0 },
	{ rxopen,	rxclose,	rxstrategy,	nodev,		/*12*/
	  nodev,	0,		0 },
	{ uuopen,	uuclose,	uustrategy,	nodev,		/*13*/
	  nodev,	0,		0 },
	{ rlopen,	nulldev,	rlstrategy,	nodev,		/*14*/
	  rldump,	rlsize,		0 },
	{ tmscpopen,	tmscpclose,	tmscpstrategy,	tmscpioctl,	/*15*/
	  tmscpdump,	0,		B_TAPE },
	{ kdbopen,	nulldev,	kdbstrategy,	nodev,		/*16*/
	  kdbdump,	kdbsize,	0 },
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
int	crlopen(),crlclose(),crlrw();
#else
#define	crlopen		nodev
#define	crlclose	nodev
#define	crlrw		nodev
#endif

#if VAX8200
int	rx50open(),rx50close(),rx50rw();
#else
#define	rx50open	nodev
#define	rx50close	nodev
#define	rx50rw		nodev
#endif

#if VAX780
int	flopen(),flclose(),flrw();
#else
#define	flopen	nodev
#define	flclose	nodev
#define	flrw	nodev
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

int 	mmrw();
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

#include "qd.h"
#if NQD > 0
int	qdopen(), qdclose(), qdread(), qdwrite(), qdioctl(), qdstop(),
	qdreset(), qdselect(), qdcons_init();
#else
#define qdopen	nodev
#define qdclose	nodev
#define qdread	nodev
#define qdwrite	nodev
#define qdioctl	nodev
#define qdstop	nodev
#define qdreset	nulldev
#define qdselect	nodev
#define qdcons_init	nodev
#endif

#if defined(INGRES)
int	iiioctl(), iiclose(), iiopen();
#else
#define iiopen nodev
#define iiclose nodev
#define iiioctl nodev
#endif

#ifdef	DATAKIT
#include "datakit.h"
#include "dktty.h"
#include "kmc.h"
#endif

#if !defined(NDATAKIT) || NDATAKIT == 0
#define	dkopen	nodev
#define	dkclose	nodev
#define	dkread	nodev
#define	dkwrite	nodev
#define	dkioctl	nodev
#else
int	dkopen(),dkclose(),dkread(),dkwrite(),dkioctl();
#endif

#if !defined(NDKTTY) || NDKTTY == 0
#define	dktopen		nodev
#define	dktclose	nodev
#define	dktread		nodev
#define	dktwrite	nodev
#define	dktioctl	nodev
#define	dktstop		nulldev
#define	dkt		0
#else
int	dktopen(),dktclose(),dktread(),dktwrite(),dktioctl(), dktstop();
struct tty dkt[];
#endif

#if NKMC > 0
int kmcopen(), kmcclose(), kmcwrite(), kmcioctl(), kmcread();
int kmcrint(), kmcload(), kmcset(), kmcdclr();
#else
#define kmcopen nodev
#define kmcclose nodev
#define kmcwrite nodev
#define kmcioctl nodev
#define kmcread nodev
#define kmcdclr nodev
#endif

int	logopen(), logclose(), logread(), logioctl(), logselect();

int	fdopen();

int	ttselect(), seltrue();

struct cdevsw	cdevsw[] =
{
	cnopen,		cnclose,	cnread,		cnwrite,	/*0*/
	cnioctl,	nulldev,	nulldev,	&cons,
	ttselect,	nodev,		NULL,
	dzopen,		dzclose,	dzread,		dzwrite,	/*1*/
	dzioctl,	dzstop,		dzreset,	dz_tty,
	ttselect,	nodev,		NULL,
	syopen,		nulldev,	syread,		sywrite,	/*2*/
	syioctl,	nulldev,	nulldev,	NULL,
	syselect,	nodev,		NULL,
	nulldev,	nulldev,	mmrw,		mmrw,		/*3*/
	nodev,		nulldev,	nulldev,	NULL,
	mmselect,	nodev,		NULL,
	hpopen,		hpclose,	rawread,	rawwrite,	/*4*/
	hpioctl,	nodev,		nulldev,	NULL,
	seltrue,	nodev,		hpstrategy,
	htopen,		htclose,	rawread,	rawwrite,	/*5*/
	htioctl,	nodev,		nulldev,	NULL,
	seltrue,	nodev,		htstrategy,
	vpopen,		vpclose,	nodev,		vpwrite,	/*6*/
	vpioctl,	nulldev,	vpreset,	NULL,
	vpselect,	nodev,		NULL,
	nulldev,	nulldev,	rawread,	rawwrite,	/*7*/
	nodev,		nodev,		nulldev,	NULL,
	nodev,		nodev,		swstrategy,
	flopen,		flclose,	flrw,		flrw,		/*8*/
	nodev,		nodev,		nulldev,	NULL,
	seltrue,	nodev,		NULL,
	udaopen,	udaclose,	rawread,	rawwrite,	/*9*/
	udaioctl,	nodev,		udareset,	NULL,
	seltrue,	nodev,		udastrategy,
	vaopen,		vaclose,	nodev,		vawrite,	/*10*/
	vaioctl,	nulldev,	vareset,	NULL,
	vaselect,	nodev,		NULL,
	rkopen,		nulldev,	rawread,	rawwrite,	/*11*/
	nodev,		nodev,		rkreset,	NULL,
	seltrue,	nodev,		rkstrategy,
	dhopen,		dhclose,	dhread,		dhwrite,	/*12*/
	dhioctl,	dhstop,		dhreset,	dh11,
	ttselect,	nodev,		NULL,
	upopen,		nulldev,	rawread,	rawwrite,	/*13*/
	nodev,		nodev,		upreset,	NULL,
	seltrue,	nodev,		upstrategy,
	tmopen,		tmclose,	rawread,	rawwrite,	/*14*/
	tmioctl,	nodev,		tmreset,	NULL,
	seltrue,	nodev,		tmstrategy,
	lpopen,		lpclose,	nodev,		lpwrite,	/*15*/
	nodev,		nodev,		lpreset,	NULL,
	seltrue,	nodev,		NULL,
	tsopen,		tsclose,	rawread,	rawwrite,	/*16*/
	tsioctl,	nodev,		tsreset,	NULL,
	seltrue,	nodev,		tsstrategy,
	utopen,		utclose,	rawread,	rawwrite,	/*17*/
	utioctl,	nodev,		utreset,	NULL,
	seltrue,	nodev,		utstrategy,
	ctopen,		ctclose,	nodev,		ctwrite,	/*18*/
	nodev,		nodev,		nulldev,	NULL,
	seltrue,	nodev,		NULL,
	mtopen,		mtclose,	rawread,	rawwrite,	/*19*/
	mtioctl,	nodev,		nodev,		NULL,
	seltrue,	nodev,		mtstrategy,
	ptsopen,	ptsclose,	ptsread,	ptswrite,	/*20*/
	ptyioctl,	ptsstop,	nulldev,	pt_tty,
	ttselect,	nodev,		NULL,
	ptcopen,	ptcclose,	ptcread,	ptcwrite,	/*21*/
	ptyioctl,	nulldev,	nulldev,	pt_tty,
	ptcselect,	nodev,		NULL,
	dmfopen,	dmfclose,	dmfread,	dmfwrite,	/*22*/
	dmfioctl,	dmfstop,	dmfreset,	dmf_tty,
	ttselect,	nodev,		NULL,
	idcopen,	nulldev,	rawread,	rawwrite,	/*23*/
	nodev,		nodev,		idcreset,	NULL,
	seltrue,	nodev,		idcstrategy,
	dnopen,		dnclose,	nodev,		dnwrite,	/*24*/
	nodev,		nodev,		nulldev,	NULL,
	seltrue,	nodev,		NULL,
/* 25-29 reserved to local sites */
	nodev,		nodev,		nodev,		nodev,		/*25*/
	nodev,		nulldev,	nulldev,	NULL,
	nodev,		nodev,		NULL,
	lpaopen,	lpaclose,	lparead,	lpawrite,	/*26*/
	lpaioctl,	nodev,		nulldev,	NULL,
	seltrue,	nodev,		NULL,
	psopen,		psclose,	psread,		pswrite,	/*27*/
	psioctl,	nodev,		psreset,	NULL,
	seltrue,	nodev,		NULL,
	nodev,		nodev,		nodev,		nodev,		/*28*/
	nodev,		nulldev,	nulldev,	NULL,
	nodev,		nodev,		NULL,
	adopen,		adclose,	nodev,		nodev,		/*29*/
	adioctl,	nodev,		adreset,	NULL,
	seltrue,	nodev,		NULL,
	rxopen,		rxclose,	rxread,		rxwrite,	/*30*/
	rxioctl,	nodev,		rxreset,	NULL,
	seltrue,	nodev,		NULL,
	ikopen,		ikclose,	ikread,		ikwrite,	/*31*/
	ikioctl,	nodev,		ikreset,	NULL,
	seltrue,	nodev,		NULL,
	rlopen,		nodev,		rawread,	rawwrite,	/*32*/
	nodev,		nodev,		rlreset,	NULL,
	seltrue,	nodev,		rlstrategy,
	logopen,	logclose,	logread,	nodev,		/*33*/
	logioctl,	nodev,		nulldev,	NULL,
	logselect,	nodev,		NULL,
	dhuopen,	dhuclose,	dhuread,	dhuwrite,	/*34*/
	dhuioctl,	dhustop,	dhureset,	dhu_tty,
	ttselect,	nodev,		NULL,
 	crlopen,	crlclose,	crlrw,		crlrw,		/*35*/
 	nodev,		nodev,		nulldev,	NULL,
 	seltrue,	nodev,		NULL,
	vsopen,		vsclose,	nodev,		nodev,		/*36*/
	vsioctl,	nodev,		vsreset,	NULL,
	vsselect,	nodev,		NULL,
	dmzopen,	dmzclose,	dmzread,	dmzwrite,	/*37*/
	dmzioctl,	dmzstop,	dmzreset,	dmz_tty,
	ttselect,	nodev,		NULL,
	tmscpopen,	tmscpclose,	rawread,	rawwrite,	/*38*/
	tmscpioctl,	nodev,		tmscpreset,	NULL,
	seltrue,	nodev,		tmscpstrategy,
	npopen,		npclose,	npread,		npwrite,	/*39*/
	npioctl,	nodev,		npreset,	NULL,
	seltrue,	nodev,		NULL,
	qvopen,		qvclose,	qvread,		qvwrite,	/*40*/
	qvioctl,	qvstop,		qvreset,	NULL,
	qvselect,	nodev,		NULL,
	qdopen,		qdclose,	qdread,		qdwrite,	/*41*/
	qdioctl,	qdstop,		qdreset,	NULL,
	qdselect,	nodev,		NULL,
/* 42-50 reserved to local sites */
	nodev,		nodev,		nodev,		nodev,		/*42*/
	nodev,		nulldev,	nulldev,	NULL,
	nodev,		nodev,		NULL,
	iiopen,		iiclose,	nulldev,	nulldev,	/*43*/
	iiioctl,	nulldev,	nulldev,	NULL,
	seltrue,	nodev,		NULL,
	dkopen, 	dkclose,	dkread, 	dkwrite,	/*44*/
	dkioctl,	nulldev,	nulldev,	NULL,
	seltrue,	nodev,		NULL,
	dktopen, 	dktclose,	dktread, 	dktwrite,	/*45*/
	dktioctl,	dktstop,	nulldev,	dkt,
	ttselect,	nodev,		NULL,
	kmcopen,	kmcclose,	kmcread,	kmcwrite,	/*46*/
	kmcioctl,	nulldev,	kmcdclr,	NULL,
	seltrue,	nodev,		NULL,
	nodev,		nodev,		nodev,		nodev,		/*47*/
	nodev,		nulldev,	nulldev,	NULL,
	nodev,		nodev,		NULL,
	nodev,		nodev,		nodev,		nodev,		/*48*/
	nodev,		nulldev,	nulldev,	NULL,
	nodev,		nodev,		NULL,
	nodev,		nodev,		nodev,		nodev,		/*49*/
	nodev,		nulldev,	nulldev,	NULL,
	nodev,		nodev,		NULL,
	nodev,		nodev,		nodev,		nodev,		/*50*/
	nodev,		nulldev,	nulldev,	NULL,
	nodev,		nodev,		NULL,
	rx50open,	rx50close,	rx50rw,		rx50rw,		/*51*/
	nodev,		nodev,		nulldev,	0,
	seltrue,	nodev,		NULL,
/* kdb50 ra */
	kdbopen,	nulldev/*XXX*/,	rawread,	rawwrite,	/*52*/
	nodev,		nodev,		nulldev,	0,
	seltrue,	nodev,		kdbstrategy,
	fdopen,		nodev,		nodev,		nodev,		/*53*/
	nodev,		nodev,		nodev,		NULL,
	nodev,		nodev,		NULL,
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
