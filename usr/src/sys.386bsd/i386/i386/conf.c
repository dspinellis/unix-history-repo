/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
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
 *	@(#)conf.c	5.8 (Berkeley) 5/12/91
 *
 * PATCHES MAGIC                LEVEL   PATCH THAT GOT US HERE
 * --------------------         -----   ----------------------
 * CURRENT PATCH LEVEL:         2       00080
 * --------------------         -----   ----------------------
 *
 * 10 Feb 93	Jordan K. Hubbard	Added select entry for com driver
 * 10 Feb 93    Julian Elischer		Add empty table entries
 *					so we can allocate numbers
 */
static char rcsid[] = "$Header: /usr/src/sys.386bsd/i386/i386/RCS/conf.c,v 1.2 92/01/21 14:21:57 william Exp Locker: toor $";

#include "param.h"
#include "systm.h"
#include "buf.h"
#include "ioctl.h"
#include "tty.h"
#include "conf.h"

int	nullop(), enxio(), enodev(), rawread(), rawwrite(), swstrategy();
int	rawread(), rawwrite(), swstrategy();

#include "wd.h"
#if NWD > 0
int	wdopen(),wdclose(),wdstrategy(),wdioctl();
int	wddump(),wdsize();
#else
#define	wdopen		enxio
#define	wdclose		enxio
#define	wdstrategy	enxio
#define	wdioctl		enxio
#define	wddump		enxio
#define	wdsize		NULL
#endif

#include "as.h"
#if NAS > 0
int	asopen(),asclose(),asstrategy(),asioctl();
int	/*asdump(),*/assize();
#define	asdump		enxio
#else
#define	asopen		enxio
#define	asclose		enxio
#define	asstrategy	enxio
#define	asioctl		enxio
#define	asdump		enxio
#define	assize		NULL
#endif

#include "wt.h"
#if NWT > 0
int	wtopen(),wtclose(),wtstrategy(),wtioctl();
int	wtdump(),wtsize();
#else
#define	wtopen		enxio
#define	wtclose		enxio
#define	wtstrategy	enxio
#define	wtioctl		enxio
#define	wtdump		enxio
#define	wtsize		NULL
#endif

#include "fd.h"
#if NFD > 0
int	Fdopen(),fdclose(),fdstrategy();
#define	fdioctl		enxio
#define	fddump		enxio
#define	fdsize		NULL
#else
#define	Fdopen		enxio
#define	fdclose		enxio
#define	fdstrategy	enxio
#define	fdioctl		enxio
#define	fddump		enxio
#define	fdsize		NULL
#endif

int	swstrategy(),swread(),swwrite();

struct bdevsw	bdevsw[] =
{
	{ wdopen,	wdclose,	wdstrategy,	wdioctl,	/*0*/
	  wddump,	wdsize,		NULL },
	{ enodev,	enodev,		swstrategy,	enodev,		/*1*/
	  enodev,	enodev,		NULL },
	{ Fdopen,	fdclose,	fdstrategy,	fdioctl,	/*2*/
	  fddump,	fdsize,		NULL },
	{ wtopen,	wtclose,	wtstrategy,	wtioctl,	/*3*/
	  wtdump,	wtsize,		B_TAPE },
	{ asopen,	asclose,	asstrategy,	asioctl,	/*4*/
	  asdump,	assize,		NULL },
	{ enxio,	enxio,		enxio,		enxio,		/*5*/
	  enxio,	NULL,		NULL },
	{ enxio,	enxio,		enxio,		enxio,		/*6*/
	  enxio,	NULL,		NULL },
	{ enxio,	enxio,		enxio,		enxio,		/*7*/
	  enxio,	NULL,		NULL },
	{ enxio,	enxio,		enxio,		enxio,		/*8*/
	  enxio,	NULL,		NULL },
	{ enxio,	enxio,		enxio,		enxio,		/*9*/
	  enxio,	NULL,		NULL },
	{ enxio,	enxio,		enxio,		enxio,		/*A*/
	  enxio,	NULL,		NULL },
};
int	nblkdev = sizeof (bdevsw) / sizeof (bdevsw[0]);

int	cnopen(),cnclose(),cnread(),cnwrite(),cnioctl(),cnselect();

int	pcopen(),pcclose(),pcread(),pcwrite(),pcioctl(),pcmmap();
extern	struct tty pccons;

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
#define	pt_tty		NULL
#define	ptcselect	enxio
#define	ptsstop		nullop
#endif

#include "com.h"
#if NCOM > 0
int	comopen(),comclose(),comread(),comwrite(),comioctl(),comselect();
#define comreset	enxio
extern	struct tty com_tty[];
#else
#define comopen		enxio
#define comclose	enxio
#define comread		enxio
#define comwrite	enxio
#define comioctl	enxio
#define comreset	enxio
#define comselect	enxio
#define	com_tty		NULL
#endif

int	logopen(),logclose(),logread(),logioctl(),logselect();

int	ttselect(), seltrue();


struct cdevsw	cdevsw[] =
{
	{ cnopen,	cnclose,	cnread,		cnwrite,	/*0*/
	  cnioctl,	nullop,		nullop,		NULL,
	  cnselect,	enodev,		NULL },
	{ cttyopen,	nullop,		cttyread,	cttywrite,	/*1*/
	  cttyioctl,	nullop,		nullop,		NULL,
	  cttyselect,	enodev,		NULL },
        { nullop,       nullop,         mmrw,           mmrw,           /*2*/
          enodev,       nullop,         nullop,         NULL,
          mmselect,     enodev,         NULL },
	{ wdopen,	wdclose,	rawread,	rawwrite,	/*3*/
	  wdioctl,	enodev,		nullop,		NULL,
	  seltrue,	enodev,		wdstrategy },
	{ nullop,	nullop,		rawread,	rawwrite,	/*4*/
	  enodev,	enodev,		nullop,		NULL,
	  enodev,	enodev,		swstrategy },
	{ ptsopen,	ptsclose,	ptsread,	ptswrite,	/*5*/
	  ptyioctl,	ptsstop,	nullop,		pt_tty,
	  ttselect,	enodev,		NULL },
	{ ptcopen,	ptcclose,	ptcread,	ptcwrite,	/*6*/
	  ptyioctl,	nullop,		nullop,		pt_tty,
	  ptcselect,	enodev,		NULL },
	{ logopen,	logclose,	logread,	enodev,		/*7*/
	  logioctl,	enodev,		nullop,		NULL,
	  logselect,	enodev,		NULL },
	{ comopen,	comclose,	comread,	comwrite,	/*8*/
	  comioctl,	enodev,		comreset,	com_tty,
	  comselect,	enodev,		NULL },
	{ Fdopen,	fdclose,	rawread,	rawwrite,	/*9*/
	  fdioctl,	enodev,		nullop,		NULL,
	  seltrue,	enodev,		fdstrategy },
	{ wtopen,	wtclose,	rawread,	rawwrite,	/*A*/
	  wtioctl,	enodev,		nullop,		NULL,
	  seltrue,	enodev,		wtstrategy },
	{ enodev,	enodev,		enodev,		enodev,		/*B*/
	  enodev,	enodev,		nullop,		NULL,
	  seltrue,	enodev,		enodev },
	{ pcopen,	pcclose,	pcread,		pcwrite,	/*C*/
	  pcioctl,	nullop,		nullop,		&pccons,
	  ttselect,	pcmmap,		NULL },
	{ asopen,	asclose,	rawread,	rawwrite,	/*D*/
	  asioctl,	enodev,		nullop,		NULL,	/* scsi disk*/
	  seltrue,	enodev,		asstrategy },
	{ enxio,	enxio,		enxio,		enxio,		/*E*/
	  enxio,	enxio,		enxio,		NULL,	/* scsi tape*/
	  enxio,	enxio,		enxio },
	{ enxio,	enxio,		enxio,		enxio,		/*F*/
	  enxio,	enxio,		enxio,		NULL,	/* scsi cdrom*/
	  enxio,	enxio,		enxio },
	{ enxio,	enxio,		enxio,		enxio,		/*10*/
	  enxio,	enxio,		enxio,		NULL,	/* lpr*/
	  enxio,	enxio,		enxio },
	{ enxio,	enxio,		enxio,		enxio,		/*11*/
	  enxio,	enxio,		enxio,		NULL,	/* scsi 'changer'*/
	  enxio,	enxio,		enxio },
	{ enxio,	enxio,		enxio,		enxio,		/*12*/
	  enxio,	enxio,		enxio,		NULL,	/* scsi generic */
	  enxio,	enxio,		enxio },
	{ enxio,	enxio,		enxio,		enxio,		/*13*/
	  enxio,	enxio,		enxio,		NULL,	/* xten power ctrlr*/
	  enxio,	enxio,		enxio },
	{ enxio,	enxio,		enxio,		enxio,		/*14*/
	  enxio,	enxio,		enxio,		NULL,	/* soundblaster?*/
	  enxio,	enxio,		enxio },
	{ enxio,	enxio,		enxio,		enxio,		/*15*/
	  enxio,	enxio,		enxio,		NULL,
	  enxio,	enxio,		enxio },
	{ enxio,	enxio,		enxio,		enxio,		/*16*/
	  enxio,	enxio,		enxio,		NULL,
	  enxio,	enxio,		enxio },
	{ enxio,	enxio,		enxio,		enxio,		/*17*/
	  enxio,	enxio,		enxio,		NULL,
	  enxio,	enxio,		enxio },
	{ enxio,	enxio,		enxio,		enxio,		/*18*/
	  enxio,	enxio,		enxio,		NULL,
	  enxio,	enxio,		enxio },
	{ enxio,	enxio,		enxio,		enxio,		/*19*/
	  enxio,	enxio,		enxio,		NULL,
	  enxio,	enxio,		enxio },
	{ enxio,	enxio,		enxio,		enxio,		/*1A*/
	  enxio,	enxio,		enxio,		NULL,
	  enxio,	enxio,		enxio },
	{ enxio,	enxio,		enxio,		enxio,		/*1B*/
	  enxio,	enxio,		enxio,		NULL,
	  enxio,	enxio,		enxio },
	{ enxio,	enxio,		enxio,		enxio,		/*1C*/
	  enxio,	enxio,		enxio,		NULL,
	  enxio,	enxio,		enxio },
	{ enxio,	enxio,		enxio,		enxio,		/*1D*/
	  enxio,	enxio,		enxio,		NULL,
	  enxio,	enxio,		enxio },
	{ enxio,	enxio,		enxio,		enxio,		/*1E*/
	  enxio,	enxio,		enxio,		NULL,
	  enxio,	enxio,		enxio },
	{ enxio,	enxio,		enxio,		enxio,		/*1F*/
	  enxio,	enxio,		enxio,		NULL,
	  enxio,	enxio,		enxio },
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
