/*-
 * Copyright (c) 1982, 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)conf.c	7.19 (Berkeley) %G%
 */

#include "sys/param.h"
#include "sys/systm.h"
#include "sys/buf.h"
#include "sys/ioctl.h"
#include "sys/tty.h"
#include "sys/conf.h"

int nullop(), enxio(), enodev(), rawread(), rawwrite(), swstrategy();

#include "hp.h"
#if NHP > 0
int	hpopen(),hpclose(),hpstrategy(),hpioctl(),hpdump(),hpsize();
#else
#define	hpopen		enxio
#define	hpclose		enxio
#define	hpstrategy	enxio
#define	hpioctl		enxio
#define	hpdump		enxio
#define	hpsize		0
#endif
 
#include "tu.h"
#if NHT > 0
int	htopen(),htclose(),htstrategy(),htdump(),htioctl();
#else
#define	htopen		enxio
#define	htclose		enxio
#define	htstrategy	enxio
#define	htdump		enxio
#define	htioctl		enxio
#endif

#include "rk.h"
#if NHK > 0
int	rkopen(),rkstrategy(),rkintr(),rkdump(),rkreset(),rksize();
#else
#define	rkopen		enxio
#define	rkstrategy	enxio
#define	rkintr		enxio
#define	rkdump		enxio
#define	rkreset		enxio
#define	rksize		0
#endif

#include "te.h"
#if NTE > 0
int	tmopen(),tmclose(),tmstrategy(),tmioctl(),tmdump(),tmreset();
#else
#define	tmopen		enxio
#define	tmclose		enxio
#define	tmstrategy	enxio
#define	tmioctl		enxio
#define	tmdump		enxio
#define	tmreset		nullop
#endif

#include "tms.h"
#if NTMS > 0
int	tmscpopen(),tmscpclose(),tmscpstrategy();
int	tmscpioctl(),tmscpdump(),tmscpreset();
#else
#define	tmscpopen	enxio
#define	tmscpclose	enxio
#define	tmscpstrategy	enxio
#define	tmscpioctl	enxio
#define	tmscpdump	enxio
#define	tmscpreset	nullop
#endif

#include "ts.h"
#if NTS > 0
int	tsopen(),tsclose(),tsstrategy(),tsioctl(),tsdump(),tsreset();
#else
#define	tsopen		enxio
#define	tsclose		enxio
#define	tsstrategy	enxio
#define	tsioctl		enxio
#define	tsdump		enxio
#define	tsreset		nullop
#endif

#include "mu.h"
#if NMT > 0
int	mtopen(),mtclose(),mtstrategy(),mtioctl(),mtdump();
#else
#define	mtopen		enxio
#define	mtclose		enxio
#define	mtstrategy	enxio
#define	mtioctl		enxio
#define	mtdump		enxio
#endif

#include "ra.h"
#if NUDA > 0
int	udaopen(),udaclose(),udastrategy();
int	udaioctl(),udareset(),udadump(),udasize();
#else
#define	udaopen		enxio
#define	udaclose	enxio
#define	udastrategy	enxio
#define	udaioctl	enxio
#define	udareset	nullop
#define	udadump		enxio
#define	udasize		0
#endif

#include "kra.h"
#if NKDB > 0
int	kdbopen(),kdbstrategy(),kdbdump(),kdbsize();
#else
#define	kdbopen		enxio
#define	kdbstrategy	enxio
#define	kdbdump		enxio
#define	kdbsize		0
#endif

#include "up.h"
#if NSC > 0
int	upopen(),upstrategy(),upreset(),updump(),upsize();
#else
#define	upopen		enxio
#define	upstrategy	enxio
#define	upreset		nullop
#define	updump		enxio
#define	upsize		0
#endif

#include "tj.h"
#if NUT > 0
int	utopen(),utclose(),utstrategy(),utioctl(),utreset(),utdump();
#else
#define	utopen		enxio
#define	utclose		enxio
#define	utstrategy	enxio
#define	utreset		nullop
#define	utioctl		enxio
#define	utdump		enxio
#endif

#include "rb.h"
#if NIDC > 0
int	idcopen(),idcstrategy(),idcreset(),idcdump(),idcsize();;
#else
#define	idcopen		enxio
#define	idcstrategy	enxio
#define	idcreset	nullop
#define	idcdump		enxio
#define	idcsize		0
#endif

#if defined(VAX750) || defined(VAX730)
int	tuopen(),tuclose(),tustrategy();
#else
#define	tuopen		enxio
#define	tuclose		enxio
#define	tustrategy	enxio
#endif

#include "rx.h"
#if NFX > 0
int	rxopen(),rxstrategy(),rxclose(),rxread(),rxwrite(),rxreset(),rxioctl();
#else
#define	rxopen		enxio
#define rxstrategy	enxio
#define	rxclose		enxio
#define	rxread		enxio
#define	rxwrite		enxio
#define	rxreset		nullop
#define	rxioctl		enxio
#endif

#include "uu.h"
#if NUU > 0
int	uuopen(),uustrategy(),uuclose(),uureset(),uuioctl();
#else
#define	uuopen		enxio
#define uustrategy	enxio
#define	uuclose		enxio
#define	uureset		nullop
#define	uuioctl		enxio
#endif

#include "rl.h"
#if NRL > 0
int	rlopen(),rlstrategy(),rlreset(),rldump(),rlsize();
#else
#define	rlopen		enxio
#define	rlstrategy	enxio
#define	rlreset		nullop
#define	rldump		enxio
#define	rlsize		0
#endif

#include "np.h"
#if NNP > 0
int	npopen(),npclose(),npread(),npwrite();
int	npreset(),npioctl();
#else
#define	npopen		enxio
#define	npclose		enxio
#define	npread		enxio
#define	npwrite		enxio
#define	npreset		nullop
#define	npioctl		enxio
#endif

struct bdevsw	bdevsw[] =
{
	{ hpopen,	hpclose,	hpstrategy,	hpioctl,	/*0*/
	  hpdump,	hpsize,		0 },
	{ htopen,	htclose,	htstrategy,	htioctl,	/*1*/
	  htdump,	0,		B_TAPE },
	{ upopen,	nullop,	upstrategy,	enodev,		/*2*/
	  updump,	upsize,		0 },
	{ rkopen,	nullop,	rkstrategy,	enodev,		/*3*/
	  rkdump,	rksize,		0 },
	{ enodev,	enodev,		swstrategy,	enodev,		/*4*/
	  enodev,	0,		0 },
	{ tmopen,	tmclose,	tmstrategy,	tmioctl,	/*5*/
	  tmdump,	0,		B_TAPE },
	{ tsopen,	tsclose,	tsstrategy,	tsioctl,	/*6*/
	  tsdump,	0,		B_TAPE },
	{ mtopen,	mtclose,	mtstrategy,	mtioctl,	/*7*/
	  mtdump,	0,		B_TAPE },
	{ tuopen,	tuclose,	tustrategy,	enodev,		/*8*/
	  enodev,	0,		B_TAPE },
	{ udaopen,	udaclose,	udastrategy,	udaioctl,	/*9*/
	  udadump,	udasize,	0 },
	{ utopen,	utclose,	utstrategy,	utioctl,	/*10*/
	  utdump,	0,		B_TAPE },
	{ idcopen,	nullop,	idcstrategy,	enodev,		/*11*/
	  idcdump,	idcsize,	0 },
	{ rxopen,	rxclose,	rxstrategy,	enodev,		/*12*/
	  enodev,	0,		0 },
	{ uuopen,	uuclose,	uustrategy,	enodev,		/*13*/
	  enodev,	0,		0 },
	{ rlopen,	nullop,	rlstrategy,	enodev,		/*14*/
	  rldump,	rlsize,		0 },
	{ tmscpopen,	tmscpclose,	tmscpstrategy,	tmscpioctl,	/*15*/
	  tmscpdump,	0,		B_TAPE },
	{ kdbopen,	nullop,	kdbstrategy,	enodev,		/*16*/
	  kdbdump,	kdbsize,	0 },
};
int	nblkdev = sizeof (bdevsw) / sizeof (bdevsw[0]);

int	cnopen(),cnclose(),cnread(),cnwrite(),cnioctl();
struct tty cons;

#include "acc.h"
#if NACC > 0
int     accreset();
#else
#define accreset nullop
#endif

#include "ct.h"
#if NCT > 0
int	ctopen(),ctclose(),ctwrite();
#else
#define	ctopen	nullop
#define	ctclose	nullop
#define	ctwrite	nullop
#endif

#include "dh.h"
#if NDH == 0
#define	dhopen	enxio
#define	dhclose	enxio
#define	dhread	enxio
#define	dhwrite	enxio
#define	dhioctl	enxio
#define	dhstop	enxio
#define	dhreset	nullop
#define	dh11	0
#else
int	dhopen(),dhclose(),dhread(),dhwrite(),dhioctl(),dhstop(),dhreset();
struct	tty dh11[];
#endif

#include "dmf.h"
#if NDMF == 0
#define	dmfopen		enxio
#define	dmfclose	enxio
#define	dmfread		enxio
#define	dmfwrite	enxio
#define	dmfioctl	enxio
#define	dmfstop		enxio
#define	dmfreset	nullop
#define	dmf_tty	0
#else
int	dmfopen(),dmfclose(),dmfread(),dmfwrite(),dmfioctl(),dmfstop(),dmfreset();
struct	tty dmf_tty[];
#endif

#if VAX8600
int	crlopen(),crlclose(),crlrw();
#else
#define	crlopen		enxio
#define	crlclose	enxio
#define	crlrw		enxio
#endif

#if VAX8200
int	rx50open(),rx50close(),rx50rw();
#else
#define	rx50open	enxio
#define	rx50close	enxio
#define	rx50rw		enxio
#endif

#if VAX780
int	flopen(),flclose(),flrw();
#else
#define	flopen	enxio
#define	flclose	enxio
#define	flrw	enxio
#endif

#include "dz.h"
#if NDZ == 0
#define	dzopen	enxio
#define	dzclose	enxio
#define	dzread	enxio
#define	dzwrite	enxio
#define	dzioctl	enxio
#define	dzstop	enxio
#define	dzreset	nullop
#define	dz_tty	0
#else
int	dzopen(),dzclose(),dzread(),dzwrite(),dzioctl(),dzstop(),dzreset();
struct	tty dz_tty[];
#endif

#include "lp.h"
#if NLP > 0
int	lpopen(),lpclose(),lpwrite(),lpreset();
#else
#define	lpopen		enxio
#define	lpclose		enxio
#define	lpwrite		enxio
#define	lpreset		nullop
#endif

int	cttyopen(),cttyread(),cttywrite(),cttyioctl(),cttyselect();

int 	mmrw();
#define	mmselect	seltrue

#include "va.h"
#if NVA > 0
int	vaopen(),vaclose(),vawrite(),vaioctl(),vareset(),vaselect();
#else
#define	vaopen		enxio
#define	vaclose		enxio
#define	vawrite		enxio
#define	vaopen		enxio
#define	vaioctl		enxio
#define	vareset		nullop
#define	vaselect	enxio
#endif

#include "vp.h"
#if NVP > 0
int	vpopen(),vpclose(),vpwrite(),vpioctl(),vpreset(),vpselect();
#else
#define	vpopen		enxio
#define	vpclose		enxio
#define	vpwrite		enxio
#define	vpioctl		enxio
#define	vpreset		nullop
#define	vpselect	enxio
#endif

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

#include "lpa.h"
#if NLPA > 0
int	lpaopen(),lpaclose(),lparead(),lpawrite(),lpaioctl();
#else
#define	lpaopen		enxio
#define	lpaclose	enxio
#define	lparead		enxio
#define	lpawrite	enxio
#define	lpaioctl	enxio
#endif

#include "dn.h"
#if NDN > 0
int	dnopen(),dnclose(),dnwrite();
#else
#define	dnopen		enxio
#define	dnclose		enxio
#define	dnwrite		enxio
#endif

#include "ik.h"
#if NIK > 0
int	ikopen(),ikclose(),ikread(),ikwrite(),ikioctl(),ikreset();
#else
#define ikopen enxio
#define ikclose enxio
#define ikread enxio
#define ikwrite enxio
#define ikioctl enxio
#define ikreset nullop
#endif

#include "ps.h"
#if NPS > 0
int	psopen(),psclose(),psread(),pswrite(),psioctl(),psreset();
#else
#define psopen enxio
#define psclose enxio
#define psread enxio
#define pswrite enxio
#define psopen enxio
#define psioctl enxio
#define psreset nullop
#endif

#include "ad.h"
#if NAD > 0
int	adopen(),adclose(),adioctl(),adreset();
#else
#define adopen enxio
#define adclose enxio
#define adioctl enxio
#define adreset nullop
#endif

#include "dhu.h"
#if NDHU > 0
int dhuopen(),dhuclose(),dhuread(),dhuwrite(),dhuioctl(),dhustop(),dhureset();
struct tty dhu_tty[];
#else
#define dhuopen enxio
#define dhuclose enxio
#define dhuread enxio
#define dhuwrite enxio
#define dhuioctl enxio
#define dhustop enxio
#define dhureset nullop
#define dhu_tty 0
#endif

#include "vs.h"
#if NVS > 0
int	vsopen(),vsclose(),vsioctl(),vsreset(),vsselect();
#else
#define vsopen enxio
#define vsclose enxio
#define vsioctl enxio
#define vsreset enxio
#define vsselect enxio
#endif

#include "dmz.h"
#if NDMZ > 0
int dmzopen(),dmzclose(),dmzread(),dmzwrite(),dmzioctl(),dmzstop(),dmzreset();
struct tty dmz_tty[];
#else
#define dmzopen enxio
#define dmzclose enxio
#define dmzread enxio
#define dmzwrite enxio
#define dmzioctl enxio
#define dmzstop enxio
#define dmzreset nullop
#define dmz_tty 0
#endif

#include "qv.h"
#if NQV > 0
int	qvopen(), qvclose(), qvread(), qvwrite(), qvioctl(), qvstop(),
	qvreset(), qvselect(), qvcons_init();
#else
#define qvopen	enxio
#define qvclose	enxio
#define qvread	enxio
#define qvwrite	enxio
#define qvioctl	enxio
#define qvstop	enxio
#define qvreset	nullop
#define qvselect	enxio
#define qvcons_init	enxio
#endif

#include "qd.h"
#if NQD > 0
int	qdopen(), qdclose(), qdread(), qdwrite(), qdioctl(), qdstop(),
	qdreset(), qdselect(), qdcons_init();
#else
#define qdopen	enxio
#define qdclose	enxio
#define qdread	enxio
#define qdwrite	enxio
#define qdioctl	enxio
#define qdstop	enxio
#define qdreset	nullop
#define qdselect	enxio
#define qdcons_init	enxio
#endif

#if defined(INGRES)
int	iiioctl(), iiclose(), iiopen();
#else
#define iiopen enxio
#define iiclose enxio
#define iiioctl enxio
#endif

#ifdef	DATAKIT
#include "datakit.h"
#include "dktty.h"
#include "kmc.h"
#endif

#if !defined(NDATAKIT) || NDATAKIT == 0
#define	dkopen	enxio
#define	dkclose	enxio
#define	dkread	enxio
#define	dkwrite	enxio
#define	dkioctl	enxio
#else
int	dkopen(),dkclose(),dkread(),dkwrite(),dkioctl();
#endif

#if !defined(NDKTTY) || NDKTTY == 0
#define	dktopen		enxio
#define	dktclose	enxio
#define	dktread		enxio
#define	dktwrite	enxio
#define	dktioctl	enxio
#define	dktstop		nullop
#define	dkt		0
#else
int	dktopen(),dktclose(),dktread(),dktwrite(),dktioctl(), dktstop();
struct tty dkt[];
#endif

#if NKMC > 0
int kmcopen(), kmcclose(), kmcwrite(), kmcioctl(), kmcread();
int kmcrint(), kmcload(), kmcset(), kmcdclr();
#else
#define kmcopen enxio
#define kmcclose enxio
#define kmcwrite enxio
#define kmcioctl enxio
#define kmcread enxio
#define kmcdclr enxio
#endif

int	logopen(), logclose(), logread(), logioctl(), logselect();

int	fdopen();

int	ttselect(), seltrue();

struct cdevsw	cdevsw[] =
{
	cnopen,		cnclose,	cnread,		cnwrite,	/*0*/
	cnioctl,	nullop,	nullop,	&cons,
	ttselect,	enodev,		NULL,
	dzopen,		dzclose,	dzread,		dzwrite,	/*1*/
	dzioctl,	dzstop,		dzreset,	dz_tty,
	ttselect,	enodev,		NULL,
	cttyopen,		nullop,	cttyread,		cttywrite,	/*2*/
	cttyioctl,	nullop,	nullop,	NULL,
	cttyselect,	enodev,		NULL,
	nullop,	nullop,	mmrw,		mmrw,		/*3*/
	enodev,		nullop,	nullop,	NULL,
	mmselect,	enodev,		NULL,
	hpopen,		hpclose,	rawread,	rawwrite,	/*4*/
	hpioctl,	enodev,		nullop,	NULL,
	seltrue,	enodev,		hpstrategy,
	htopen,		htclose,	rawread,	rawwrite,	/*5*/
	htioctl,	enodev,		nullop,	NULL,
	seltrue,	enodev,		htstrategy,
	vpopen,		vpclose,	enodev,		vpwrite,	/*6*/
	vpioctl,	nullop,	vpreset,	NULL,
	vpselect,	enodev,		NULL,
	nullop,	nullop,	rawread,	rawwrite,	/*7*/
	enodev,		enodev,		nullop,	NULL,
	enodev,		enodev,		swstrategy,
	flopen,		flclose,	flrw,		flrw,		/*8*/
	enodev,		enodev,		nullop,	NULL,
	seltrue,	enodev,		NULL,
	udaopen,	udaclose,	rawread,	rawwrite,	/*9*/
	udaioctl,	enodev,		udareset,	NULL,
	seltrue,	enodev,		udastrategy,
	vaopen,		vaclose,	enodev,		vawrite,	/*10*/
	vaioctl,	nullop,	vareset,	NULL,
	vaselect,	enodev,		NULL,
	rkopen,		nullop,	rawread,	rawwrite,	/*11*/
	enodev,		enodev,		rkreset,	NULL,
	seltrue,	enodev,		rkstrategy,
	dhopen,		dhclose,	dhread,		dhwrite,	/*12*/
	dhioctl,	dhstop,		dhreset,	dh11,
	ttselect,	enodev,		NULL,
	upopen,		nullop,	rawread,	rawwrite,	/*13*/
	enodev,		enodev,		upreset,	NULL,
	seltrue,	enodev,		upstrategy,
	tmopen,		tmclose,	rawread,	rawwrite,	/*14*/
	tmioctl,	enodev,		tmreset,	NULL,
	seltrue,	enodev,		tmstrategy,
	lpopen,		lpclose,	enodev,		lpwrite,	/*15*/
	enodev,		enodev,		lpreset,	NULL,
	seltrue,	enodev,		NULL,
	tsopen,		tsclose,	rawread,	rawwrite,	/*16*/
	tsioctl,	enodev,		tsreset,	NULL,
	seltrue,	enodev,		tsstrategy,
	utopen,		utclose,	rawread,	rawwrite,	/*17*/
	utioctl,	enodev,		utreset,	NULL,
	seltrue,	enodev,		utstrategy,
	ctopen,		ctclose,	enodev,		ctwrite,	/*18*/
	enodev,		enodev,		nullop,	NULL,
	seltrue,	enodev,		NULL,
	mtopen,		mtclose,	rawread,	rawwrite,	/*19*/
	mtioctl,	enodev,		enodev,		NULL,
	seltrue,	enodev,		mtstrategy,
	ptsopen,	ptsclose,	ptsread,	ptswrite,	/*20*/
	ptyioctl,	ptsstop,	nullop,	pt_tty,
	ttselect,	enodev,		NULL,
	ptcopen,	ptcclose,	ptcread,	ptcwrite,	/*21*/
	ptyioctl,	nullop,	nullop,	pt_tty,
	ptcselect,	enodev,		NULL,
	dmfopen,	dmfclose,	dmfread,	dmfwrite,	/*22*/
	dmfioctl,	dmfstop,	dmfreset,	dmf_tty,
	ttselect,	enodev,		NULL,
	idcopen,	nullop,	rawread,	rawwrite,	/*23*/
	enodev,		enodev,		idcreset,	NULL,
	seltrue,	enodev,		idcstrategy,
	dnopen,		dnclose,	enodev,		dnwrite,	/*24*/
	enodev,		enodev,		nullop,	NULL,
	seltrue,	enodev,		NULL,
/* 25-29 reserved to local sites */
	enodev,		enodev,		enodev,		enodev,		/*25*/
	enodev,		nullop,	nullop,	NULL,
	enodev,		enodev,		NULL,
	lpaopen,	lpaclose,	lparead,	lpawrite,	/*26*/
	lpaioctl,	enodev,		nullop,	NULL,
	seltrue,	enodev,		NULL,
	psopen,		psclose,	psread,		pswrite,	/*27*/
	psioctl,	enodev,		psreset,	NULL,
	seltrue,	enodev,		NULL,
	enodev,		enodev,		enodev,		enodev,		/*28*/
	enodev,		nullop,	nullop,	NULL,
	enodev,		enodev,		NULL,
	adopen,		adclose,	enodev,		enodev,		/*29*/
	adioctl,	enodev,		adreset,	NULL,
	seltrue,	enodev,		NULL,
	rxopen,		rxclose,	rxread,		rxwrite,	/*30*/
	rxioctl,	enodev,		rxreset,	NULL,
	seltrue,	enodev,		NULL,
	ikopen,		ikclose,	ikread,		ikwrite,	/*31*/
	ikioctl,	enodev,		ikreset,	NULL,
	seltrue,	enodev,		NULL,
	rlopen,		enodev,		rawread,	rawwrite,	/*32*/
	enodev,		enodev,		rlreset,	NULL,
	seltrue,	enodev,		rlstrategy,
	logopen,	logclose,	logread,	enodev,		/*33*/
	logioctl,	enodev,		nullop,	NULL,
	logselect,	enodev,		NULL,
	dhuopen,	dhuclose,	dhuread,	dhuwrite,	/*34*/
	dhuioctl,	dhustop,	dhureset,	dhu_tty,
	ttselect,	enodev,		NULL,
 	crlopen,	crlclose,	crlrw,		crlrw,		/*35*/
 	enodev,		enodev,		nullop,	NULL,
 	seltrue,	enodev,		NULL,
	vsopen,		vsclose,	enodev,		enodev,		/*36*/
	vsioctl,	enodev,		vsreset,	NULL,
	vsselect,	enodev,		NULL,
	dmzopen,	dmzclose,	dmzread,	dmzwrite,	/*37*/
	dmzioctl,	dmzstop,	dmzreset,	dmz_tty,
	ttselect,	enodev,		NULL,
	tmscpopen,	tmscpclose,	rawread,	rawwrite,	/*38*/
	tmscpioctl,	enodev,		tmscpreset,	NULL,
	seltrue,	enodev,		tmscpstrategy,
	npopen,		npclose,	npread,		npwrite,	/*39*/
	npioctl,	enodev,		npreset,	NULL,
	seltrue,	enodev,		NULL,
	qvopen,		qvclose,	qvread,		qvwrite,	/*40*/
	qvioctl,	qvstop,		qvreset,	NULL,
	qvselect,	enodev,		NULL,
	qdopen,		qdclose,	qdread,		qdwrite,	/*41*/
	qdioctl,	qdstop,		qdreset,	NULL,
	qdselect,	enodev,		NULL,
/* 42-50 reserved to local sites */
	enodev,		enodev,		enodev,		enodev,		/*42*/
	enodev,		nullop,	nullop,	NULL,
	enodev,		enodev,		NULL,
	iiopen,		iiclose,	nullop,	nullop,	/*43*/
	iiioctl,	nullop,	nullop,	NULL,
	seltrue,	enodev,		NULL,
	dkopen, 	dkclose,	dkread, 	dkwrite,	/*44*/
	dkioctl,	nullop,	nullop,	NULL,
	seltrue,	enodev,		NULL,
	dktopen, 	dktclose,	dktread, 	dktwrite,	/*45*/
	dktioctl,	dktstop,	nullop,	dkt,
	ttselect,	enodev,		NULL,
	kmcopen,	kmcclose,	kmcread,	kmcwrite,	/*46*/
	kmcioctl,	nullop,	kmcdclr,	NULL,
	seltrue,	enodev,		NULL,
	enodev,		enodev,		enodev,		enodev,		/*47*/
	enodev,		nullop,	nullop,	NULL,
	enodev,		enodev,		NULL,
	enodev,		enodev,		enodev,		enodev,		/*48*/
	enodev,		nullop,	nullop,	NULL,
	enodev,		enodev,		NULL,
	enodev,		enodev,		enodev,		enodev,		/*49*/
	enodev,		nullop,	nullop,	NULL,
	enodev,		enodev,		NULL,
	enodev,		enodev,		enodev,		enodev,		/*50*/
	enodev,		nullop,	nullop,	NULL,
	enodev,		enodev,		NULL,
	rx50open,	rx50close,	rx50rw,		rx50rw,		/*51*/
	enodev,		enodev,		nullop,	0,
	seltrue,	enodev,		NULL,
/* kdb50 ra */
	kdbopen,	nullop/*XXX*/,	rawread,	rawwrite,	/*52*/
	enodev,		enodev,		nullop,	0,
	seltrue,	enodev,		kdbstrategy,
	fdopen,		enodev,		enodev,		enodev,		/*53*/
	enodev,		enodev,		enodev,		NULL,
	enodev,		enodev,		NULL,
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
