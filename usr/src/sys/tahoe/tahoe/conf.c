/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)conf.c	7.9 (Berkeley) 5/8/91
 */

#include "sys/param.h"
#include "sys/systm.h"
#include "sys/buf.h"
#include "sys/ioctl.h"
#include "sys/tty.h"
#include "sys/conf.h"

int nullop(), enxio(), enodev(), rawread(), rawwrite(), swstrategy();

#include "dk.h"
#if NVD > 0
int	vdopen(),vdclose(),vdstrategy(),vdioctl();
int	vddump(),vdsize();
#else
#define	vdopen		enxio
#define	vdclose		enxio
#define	vdstrategy	enxio
#define	vdioctl		enxio
#define	vddump		enxio
#define	vdsize		0
#endif

#include "hd.h"
#if NHD > 0
int	hdopen(),hdclose(),hdstrategy(),hdioctl();
int	hddump(),hdsize();
#else
#define	hdopen		enxio
#define	hdclose		enxio
#define	hdstrategy	enxio
#define	hdioctl		enxio
#define	hddump		enxio
#define	hdsize		0
#endif

#include "yc.h"
#if NCY > 0
int	cyopen(),cyclose(),cystrategy(),cydump();
int	cyioctl(),cyreset();
#else
#define	cyopen		enxio
#define	cyclose		enxio
#define	cystrategy	enxio
#define	cydump		enxio
#define	cyioctl		enxio
#define	cyreset		nullop
#endif

struct bdevsw	bdevsw[] =
{
	{ enodev,	nullop,		enodev,		enodev,		/*0*/
	  enodev,	0,		0 },
	{ vdopen,	vdclose,	vdstrategy,	vdioctl,	/*1*/
	  vddump,	vdsize,		0 },
	{ hdopen,	hdclose,	hdstrategy,	hdioctl,	/*2*/
	  hddump,	hdsize,		0 },
	{ cyopen,	cyclose,	cystrategy,	cyioctl,	/*3*/
	  cydump,	0,		B_TAPE },
	{ enodev,	enodev,		swstrategy,	enodev,		/*4*/
	  enodev,	0,		0 },
};
int	nblkdev = sizeof (bdevsw) / sizeof (bdevsw[0]);

int	cnopen(),cnclose(),cnread(),cnwrite(),cnioctl();
extern	struct tty cons;

#include "vx.h"
#if NVX == 0
#define	vxopen	enxio
#define	vxclose	enxio
#define	vxread	enxio
#define	vxwrite	enxio
#define	vxioctl	enxio
#define	vxstop	enxio
#define	vxreset	nullop
#define	vx_tty	0
#else
int	vxopen(),vxclose(),vxread(),vxwrite(),vxioctl(),vxstop(),vxreset();
struct	tty vx_tty[];
#endif

int	cttyopen(),cttyread(),cttywrite(),cttyioctl(),cttyselect();

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

#include "mp.h"
#if NMP > 0
int	mpopen(), mpclose(), mpread(), mpwrite(), mpioctl(), mpstop();
int	mpdlopen(), mpdlclose(), mpdlwrite(), mpdlioctl();
extern	struct tty mp_tty[];
#else
#define	mpopen		enxio
#define	mpclose		enxio
#define	mpread		enxio
#define	mpwrite		enxio
#define	mpioctl		enxio
#define	mpstop		enxio
#define	mpdlopen	enxio
#define	mpdlclose	enxio
#define	mpdlwrite	enxio
#define	mpdlioctl	enxio
#define	mp_tty		0
#endif

#if NII > 0
int	iiioctl(), iiclose(), iiopen();
#else
#define	iiopen	enxio
#define	iiclose	enxio
#define	iiioctl	enxio
#endif

#include "enp.h"
#if NENP > 0
int	enpr_open(), enpr_close(), enpr_read(), enpr_write(), enpr_ioctl();
#else
#define enpr_open	enxio
#define enpr_close	enxio
#define enpr_read	enxio
#define enpr_write	enxio
#define enpr_ioctl	enxio
#endif

#include "dr.h"
#if NDR > 0
int     dropen(),drclose(),drread(),drwrite(),drioctl(),drreset();
#else
#define dropen enxio
#define drclose enxio
#define drread enxio
#define drwrite enxio
#define drioctl enxio
#define drreset enxio
#endif

#include "ik.h"
#if NIK > 0
int     ikopen(),ikclose(),ikread(),ikwrite(),ikioctl();
#else
#define ikopen enxio
#define ikclose enxio
#define ikread enxio
#define ikwrite enxio
#define ikioctl enxio
#endif

int	logopen(),logclose(),logread(),logioctl(),logselect();

int	fdopen();

int	ttselect(), seltrue();

struct cdevsw	cdevsw[] =
{
	{ cnopen,	cnclose,	cnread,		cnwrite,	/*0*/
	  cnioctl,	nullop,		nullop,		&cons,
	  ttselect,	enodev,		NULL },
	{ vxopen,	vxclose,	vxread,		vxwrite,	/*1*/
	  vxioctl,	vxstop,		vxreset,	vx_tty,
	  ttselect,	enodev,		NULL },
	{ cttyopen,	nullop,		cttyread,	cttywrite,	/*2*/
	  cttyioctl,	nullop,		nullop,		NULL,
	  cttyselect,	enodev,		NULL },
	{ nullop,	nullop,		mmrw,		mmrw,		/*3*/
	  enodev,	nullop,		nullop,		NULL,
	  mmselect,	enodev,		NULL },
	{ enodev,	nullop,		enodev,		enodev,		/*4*/
	  enodev,	enodev,		nullop,		NULL,
	  seltrue,	enodev,		NULL },
	{ vdopen,	vdclose,	rawread,	rawwrite,	/*5*/
	  vdioctl,	enodev,		nullop,		NULL,
	  seltrue,	enodev,		vdstrategy },
	{ hdopen,	hdclose,	rawread,	rawwrite,	/*6*/
	  hdioctl,	enodev,		nullop,		NULL,
	  seltrue,	enodev,		hdstrategy },
	{ cyopen,	cyclose,	rawread,	rawwrite,	/*7*/
	  cyioctl,	enodev,		cyreset,	NULL,
	  seltrue,	enodev,		cystrategy },
	{ nullop,	nullop,		rawread,	rawwrite,	/*8*/
	  enodev,	enodev,		nullop,		NULL,
	  enodev,	enodev,		swstrategy },
	{ ptsopen,	ptsclose,	ptsread,	ptswrite,	/*9*/
	  ptyioctl,	ptsstop,	enodev,		pt_tty,
	  ttselect,	enodev,		NULL },
	{ ptcopen,	ptcclose,	ptcread,	ptcwrite,	/*10*/
	  ptyioctl,	nullop,		enodev,		pt_tty,
	  ptcselect,	enodev,		NULL },
	{ mpdlopen,	mpdlclose,	enodev,		mpdlwrite,	/*11*/
	  mpdlioctl,	enodev,		nullop,		NULL,
	  seltrue,	enodev,		NULL },
	{ mpopen,	mpclose,	mpread,		mpwrite,	/*12*/
	  mpioctl,	mpstop,		nullop,		mp_tty,
	  ttselect,	enodev,		NULL },
	{ enodev,	enodev,		enodev,		enodev,		/*13*/
	  enodev,	enodev,		nullop,		NULL,
	  enodev,	enodev,		NULL },
	{ iiopen,	iiclose,	nullop,		nullop,		/*14*/
	  iiioctl,	nullop,		nullop,		NULL,
	  seltrue,	enodev,		NULL },
	{ logopen,	logclose,	logread,	enodev,		/*15*/
	  logioctl,	enodev,		nullop,		NULL,
	  logselect,	enodev,		NULL },
	{ enpr_open,	enpr_close,	enpr_read,	enpr_write,	/*16*/
	  enpr_ioctl,	enodev,		nullop,		NULL,
	  enodev,	enodev,		NULL },
	{ enodev,	enodev,		enodev,		enodev,		/*17*/
	  enodev,	enodev,		nullop,		NULL,
	  enodev,	enodev,		NULL },
	{ dropen,	drclose,	drread,		drwrite,	/*18*/
	  drioctl,	enodev,		drreset,	NULL,
	  enodev,	enodev,		NULL },
	{ fdopen,	enodev,		enodev,		enodev,		/*19*/
	  enodev,	enodev,		enodev,		NULL,
	  enodev,	enodev,		NULL },
/* 20-30 are reserved for local use */
	{ ikopen,	ikclose,	ikread,		ikwrite,	/*20*/
	  ikioctl,	enodev,		nullop,		NULL,
	  enodev,	enodev,		NULL },
	{ fdopen,	enodev,		enodev,		enodev,		/*21*/
	  enodev,	enodev,		enodev,		NULL,
	  enodev,	enodev,		NULL },
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
