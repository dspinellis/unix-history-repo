/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell and Rick Macklem.
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
 *	@(#)pm.c	8.1 (Berkeley) 6/10/93
 */

/* 
 *  devGraphics.c --
 *
 *     	This file contains machine-dependent routines for the graphics device.
 *
 *	Copyright (C) 1989 Digital Equipment Corporation.
 *	Permission to use, copy, modify, and distribute this software and
 *	its documentation for any purpose and without fee is hereby granted,
 *	provided that the above copyright notice appears in all copies.  
 *	Digital Equipment Corporation makes no representations about the
 *	suitability of this software for any purpose.  It is provided "as is"
 *	without express or implied warranty.
 *
 * from: $Header: /sprite/src/kernel/dev/ds3100.md/RCS/devGraphics.c,
 *	v 9.2 90/02/13 22:16:24 shirriff Exp $ SPRITE (DECWRL)";
 */

#include <pm.h>
#include <dc.h>
#if NPM > 0
#if NDC == 0
pm needs dc device
#else

#include <sys/param.h>
#include <sys/time.h>
#include <sys/kernel.h>
#include <sys/ioctl.h>
#include <sys/file.h>
#include <sys/errno.h>
#include <sys/proc.h>
#include <sys/mman.h>

#include <vm/vm.h>

#include <machine/machConst.h>
#include <machine/dc7085cons.h>
#include <machine/pmioctl.h>

#include <pmax/pmax/kn01.h>
#include <pmax/pmax/pmaxtype.h>
#include <pmax/pmax/cons.h>

#include <pmax/dev/device.h>
#include <pmax/dev/pmreg.h>
#include <pmax/dev/fbreg.h>

/*
 * These need to be mapped into user space.
 */
struct fbuaccess pmu;
struct pmax_fb pmfb;
static u_short curReg;		/* copy of PCCRegs.cmdr since it's read only */

/*
 * Forward references.
 */
static void pmScreenInit();
static void pmLoadCursor();
static void pmRestoreCursorColor();
static void pmCursorColor();
void pmPosCursor();
static void pmInitColorMap();
static void pmVDACInit();
static void pmLoadColorMap();

void pmKbdEvent(), pmMouseEvent(), pmMouseButtons();
extern void dcPutc();
extern void (*dcDivertXInput)();
extern void (*dcMouseEvent)();
extern void (*dcMouseButtons)();
extern int pmax_boardtype;
extern u_short defCursor[32];
extern struct consdev cn_tab;

int	pmprobe();
struct	driver pmdriver = {
	"pm", pmprobe, 0, 0,
};

/*
 * Test to see if device is present.
 * Return true if found and initialized ok.
 */
/*ARGSUSED*/
pmprobe(cp)
	register struct pmax_ctlr *cp;
{
	register struct pmax_fb *fp = &pmfb;

	if (pmax_boardtype != DS_PMAX)
		return (0);
	if (!fp->initialized && !pminit())
		return (0);
	if (fp->isMono)
		printf("pm0 (monochrome display)\n");
	else
		printf("pm0 (color display)\n");
	return (1);
}

/*ARGSUSED*/
pmopen(dev, flag)
	dev_t dev;
	int flag;
{
	register struct pmax_fb *fp = &pmfb;
	int s;

	if (!fp->initialized)
		return (ENXIO);
	if (fp->GraphicsOpen)
		return (EBUSY);

	fp->GraphicsOpen = 1;
	if (!fp->isMono)
		pmInitColorMap();
	/*
	 * Set up event queue for later
	 */
	fp->fbu->scrInfo.qe.eSize = PM_MAXEVQ;
	fp->fbu->scrInfo.qe.eHead = fp->fbu->scrInfo.qe.eTail = 0;
	fp->fbu->scrInfo.qe.tcSize = MOTION_BUFFER_SIZE;
	fp->fbu->scrInfo.qe.tcNext = 0;
	fp->fbu->scrInfo.qe.timestamp_ms = TO_MS(time);
	s = spltty();
	dcDivertXInput = pmKbdEvent;
	dcMouseEvent = pmMouseEvent;
	dcMouseButtons = pmMouseButtons;
	splx(s);
	return (0);
}

/*ARGSUSED*/
pmclose(dev, flag)
	dev_t dev;
	int flag;
{
	register struct pmax_fb *fp = &pmfb;
	int s;

	if (!fp->GraphicsOpen)
		return (EBADF);

	fp->GraphicsOpen = 0;
	if (!fp->isMono)
		pmInitColorMap();
	s = spltty();
	dcDivertXInput = (void (*)())0;
	dcMouseEvent = (void (*)())0;
	dcMouseButtons = (void (*)())0;
	splx(s);
	pmScreenInit();
	bzero((caddr_t)fp->fr_addr,
		(fp->isMono ? 1024 / 8 : 1024) * 864);
	pmPosCursor(fp->col * 8, fp->row * 15);
	return (0);
}

/*ARGSUSED*/
pmioctl(dev, cmd, data, flag, p)
	dev_t dev;
	caddr_t data;
	struct proc *p;
{
	register PCCRegs *pcc = (PCCRegs *)MACH_PHYS_TO_UNCACHED(KN01_SYS_PCC);
	register struct pmax_fb *fp = &pmfb;
	int s;

	switch (cmd) {
	case QIOCGINFO:
		return (fbmmap(fp, dev, data, p));

	case QIOCPMSTATE:
		/*
		 * Set mouse state.
		 */
		fp->fbu->scrInfo.mouse = *(pmCursor *)data;
		pmPosCursor(fp->fbu->scrInfo.mouse.x, fp->fbu->scrInfo.mouse.y);
		break;

	case QIOCINIT:
		/*
		 * Initialize the screen.
		 */
		pmScreenInit();
		break;

	case QIOCKPCMD:
	    {
		pmKpCmd *kpCmdPtr;
		unsigned char *cp;

		kpCmdPtr = (pmKpCmd *)data;
		if (kpCmdPtr->nbytes == 0)
			kpCmdPtr->cmd |= 0x80;
		if (!fp->GraphicsOpen)
			kpCmdPtr->cmd |= 1;
		(*fp->KBDPutc)(fp->kbddev, (int)kpCmdPtr->cmd);
		cp = &kpCmdPtr->par[0];
		for (; kpCmdPtr->nbytes > 0; cp++, kpCmdPtr->nbytes--) {
			if (kpCmdPtr->nbytes == 1)
				*cp |= 0x80;
			(*fp->KBDPutc)(fp->kbddev, (int)*cp);
		}
		break;
	    }

	case QIOCADDR:
		*(PM_Info **)data = &fp->fbu->scrInfo;
		break;

	case QIOWCURSOR:
		pmLoadCursor((unsigned short *)data);
		break;

	case QIOWCURSORCOLOR:
		pmCursorColor((unsigned int *)data);
		break;

	case QIOSETCMAP:
		pmLoadColorMap((ColorMap *)data);
		break;

	case QIOKERNLOOP:
		s = spltty();
		dcDivertXInput = pmKbdEvent;
		dcMouseEvent = pmMouseEvent;
		dcMouseButtons = pmMouseButtons;
		splx(s);
		break;

	case QIOKERNUNLOOP:
		s = spltty();
		dcDivertXInput = (void (*)())0;
		dcMouseEvent = (void (*)())0;
		dcMouseButtons = (void (*)())0;
		splx(s);
		break;

	case QIOVIDEOON:
		if (!fp->isMono)
			pmRestoreCursorColor();
		curReg |= PCC_ENPA;
		curReg &= ~PCC_FOPB;
		pcc->cmdr = curReg;
		break;

	case QIOVIDEOOFF:
		if (!fp->isMono)
			pmVDACInit();
		curReg |= PCC_FOPB;
		curReg &= ~PCC_ENPA;
		pcc->cmdr = curReg;
		break;

	default:
		printf("pm0: Unknown ioctl command %x\n", cmd);
		return (EINVAL);
	}
	return (0);
}

/*
 * Return the physical page number that corresponds to byte offset 'off'.
 */
/*ARGSUSED*/
pmmap(dev, off, prot)
	dev_t dev;
{
	int len;

	len = pmax_round_page(((vm_offset_t)&pmu & PGOFSET) + sizeof(pmu));
	if (off < len)
		return pmax_btop(MACH_CACHED_TO_PHYS(&pmu) + off);
	off -= len;
	if (off >= pmfb.fr_size)
		return (-1);
	return pmax_btop(MACH_UNCACHED_TO_PHYS(pmfb.fr_addr) + off);
}

pmselect(dev, flag, p)
	dev_t dev;
	int flag;
	struct proc *p;
{
	struct pmax_fb *fp = &pmfb;

	switch (flag) {
	case FREAD:
		if (fp->fbu->scrInfo.qe.eHead != fp->fbu->scrInfo.qe.eTail)
			return (1);
		selrecord(p, &fp->selp);
		break;
	}

	return (0);
}

static u_char	bg_RGB[3];	/* background color for the cursor */
static u_char	fg_RGB[3];	/* foreground color for the cursor */

/*
 * Test to see if device is present.
 * Return true if found and initialized ok.
 */
pminit()
{
	register PCCRegs *pcc = (PCCRegs *)MACH_PHYS_TO_UNCACHED(KN01_SYS_PCC);
	register struct pmax_fb *fp = &pmfb;

	fp->isMono = *(volatile u_short *)MACH_PHYS_TO_UNCACHED(KN01_SYS_CSR) &
		KN01_CSR_MONO;
	fp->fr_addr = (char *)MACH_PHYS_TO_UNCACHED(KN01_PHYS_FBUF_START);
	fp->fr_size = fp->isMono ? 0x40000 : 0x100000;
	/*
	 * Must be in Uncached space since the fbuaccess structure is
	 * mapped into the user's address space uncached.
	 */
	fp->fbu = (struct fbuaccess *)
		MACH_PHYS_TO_UNCACHED(MACH_CACHED_TO_PHYS(&pmu));
	fp->posCursor = pmPosCursor;
	fp->KBDPutc = dcPutc;
	fp->kbddev = makedev(DCDEV, DCKBD_PORT);
	if (fp->isMono) {
		/* check for no frame buffer */
		if (badaddr((char *)fp->fr_addr, 4))
			return (0);
	}

	/*
	 * Initialize the screen.
	 */
	pcc->cmdr = PCC_FOPB | PCC_VBHI;

	/*
	 * Initialize the cursor register.
	 */
	pcc->cmdr = curReg = PCC_ENPA | PCC_ENPB;

	/*
	 * Initialize screen info.
	 */
	fp->fbu->scrInfo.max_row = 56;
	fp->fbu->scrInfo.max_col = 80;
	fp->fbu->scrInfo.max_x = 1024;
	fp->fbu->scrInfo.max_y = 864;
	fp->fbu->scrInfo.max_cur_x = 1023;
	fp->fbu->scrInfo.max_cur_y = 863;
	fp->fbu->scrInfo.version = 11;
	fp->fbu->scrInfo.mthreshold = 4;	
	fp->fbu->scrInfo.mscale = 2;
	fp->fbu->scrInfo.min_cur_x = -15;
	fp->fbu->scrInfo.min_cur_y = -15;
	fp->fbu->scrInfo.qe.timestamp_ms = TO_MS(time);
	fp->fbu->scrInfo.qe.eSize = PM_MAXEVQ;
	fp->fbu->scrInfo.qe.eHead = fp->fbu->scrInfo.qe.eTail = 0;
	fp->fbu->scrInfo.qe.tcSize = MOTION_BUFFER_SIZE;
	fp->fbu->scrInfo.qe.tcNext = 0;

	/*
	 * Initialize the color map, the screen, and the mouse.
	 */
	pmInitColorMap();
	pmScreenInit();
	fbScroll(fp);

	fp->initialized = 1;
	if (cn_tab.cn_fb == (struct pmax_fb *)0)
		cn_tab.cn_fb = fp;
	return (1);
}	

/*
 * ----------------------------------------------------------------------------
 *
 * pmScreenInit --
 *
 *	Initialize the screen.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The screen is initialized.
 *
 * ----------------------------------------------------------------------------
 */
static void
pmScreenInit()
{
	register struct pmax_fb *fp = &pmfb;

	/*
	 * Home the cursor.
	 * We want an LSI terminal emulation.  We want the graphics
	 * terminal to scroll from the bottom. So start at the bottom.
	 */
	fp->row = 55;
	fp->col = 0;

	/*
	 * Load the cursor with the default values
	 *
	 */
	pmLoadCursor(defCursor);
}

/*
 * ----------------------------------------------------------------------------
 *
 * pmLoadCursor --
 *
 *	Routine to load the cursor Sprite pattern.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The cursor is loaded into the hardware cursor.
 *
 * ----------------------------------------------------------------------------
 */
static void
pmLoadCursor(cur)
	unsigned short *cur;
{
	register PCCRegs *pcc = (PCCRegs *)MACH_PHYS_TO_UNCACHED(KN01_SYS_PCC);
	register int i;

	curReg |= PCC_LODSA;
	pcc->cmdr = curReg;
	for (i = 0; i < 32; i++) {
		pcc->memory = cur[i];
		MachEmptyWriteBuffer();
	}
	curReg &= ~PCC_LODSA;
	pcc->cmdr = curReg;
}

/*
 * ----------------------------------------------------------------------------
 *
 * pmRestoreCursorColor --
 *
 *	Routine to restore the color of the cursor.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 * ----------------------------------------------------------------------------
 */
static void
pmRestoreCursorColor()
{
	register VDACRegs *vdac = (VDACRegs *)MACH_PHYS_TO_UNCACHED(KN01_SYS_VDAC);
	register int i;

	vdac->overWA = 0x04;
	MachEmptyWriteBuffer();
	for (i = 0; i < 3; i++) {  
		vdac->over = bg_RGB[i];
		MachEmptyWriteBuffer();
	}

	vdac->overWA = 0x08;
	MachEmptyWriteBuffer();
	vdac->over = 0x00;
	MachEmptyWriteBuffer();
	vdac->over = 0x00;
	MachEmptyWriteBuffer();
	vdac->over = 0x7f;
	MachEmptyWriteBuffer();

	vdac->overWA = 0x0c;
	MachEmptyWriteBuffer();
	for (i = 0; i < 3; i++) {
		vdac->over = fg_RGB[i];
		MachEmptyWriteBuffer();
	}
}

/*
 * ----------------------------------------------------------------------------
 *
 * pmCursorColor --
 *
 *	Set the color of the cursor.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 * ----------------------------------------------------------------------------
 */
static void
pmCursorColor(color)
	unsigned int color[];
{
	register int i, j;

	for (i = 0; i < 3; i++)
		bg_RGB[i] = (u_char)(color[i] >> 8);

	for (i = 3, j = 0; i < 6; i++, j++)
		fg_RGB[j] = (u_char)(color[i] >> 8);

	pmRestoreCursorColor();
}

/*
 * ----------------------------------------------------------------------------
 *
 * pmInitColorMap --
 *
 *	Initialize the color map.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The colormap is initialized appropriately whether it is color or 
 *	monochrome.
 *
 * ----------------------------------------------------------------------------
 */
static void
pmInitColorMap()
{
	register VDACRegs *vdac = (VDACRegs *)MACH_PHYS_TO_UNCACHED(KN01_SYS_VDAC);
	struct pmax_fb *fp = &pmfb;
	register int i;

	*(volatile char *)MACH_PHYS_TO_UNCACHED(KN01_PHYS_COLMASK_START) = 0xff;
	MachEmptyWriteBuffer();

	if (fp->isMono) {
		vdac->mapWA = 0; MachEmptyWriteBuffer();
		for (i = 0; i < 256; i++) {
			vdac->map = (i < 128) ? 0x00 : 0xff;
			MachEmptyWriteBuffer();
			vdac->map = (i < 128) ? 0x00 : 0xff;
			MachEmptyWriteBuffer();
			vdac->map = (i < 128) ? 0x00 : 0xff;
			MachEmptyWriteBuffer();
		}
	} else {
		vdac->mapWA = 0; MachEmptyWriteBuffer();
		vdac->map = 0; MachEmptyWriteBuffer();
		vdac->map = 0; MachEmptyWriteBuffer();
		vdac->map = 0; MachEmptyWriteBuffer();

		for (i = 1; i < 256; i++) {
			vdac->map = 0xff; MachEmptyWriteBuffer();
			vdac->map = 0xff; MachEmptyWriteBuffer();
			vdac->map = 0xff; MachEmptyWriteBuffer();
		}
	}

	for (i = 0; i < 3; i++) {
		bg_RGB[i] = 0x00;
		fg_RGB[i] = 0xff;
	}
	pmRestoreCursorColor();
}

/*
 * ----------------------------------------------------------------------------
 *
 * pmVDACInit --
 *
 *	Initialize the VDAC.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 * ----------------------------------------------------------------------------
 */
static void
pmVDACInit()
{
	register VDACRegs *vdac = (VDACRegs *)MACH_PHYS_TO_UNCACHED(KN01_SYS_VDAC);

	/*
	 *
	 * Initialize the VDAC
	 */
	vdac->overWA = 0x04; MachEmptyWriteBuffer();
	vdac->over = 0x00; MachEmptyWriteBuffer();
	vdac->over = 0x00; MachEmptyWriteBuffer();
	vdac->over = 0x00; MachEmptyWriteBuffer();
	vdac->overWA = 0x08; MachEmptyWriteBuffer();
	vdac->over = 0x00; MachEmptyWriteBuffer();
	vdac->over = 0x00; MachEmptyWriteBuffer();
	vdac->over = 0x7f; MachEmptyWriteBuffer();
	vdac->overWA = 0x0c; MachEmptyWriteBuffer();
	vdac->over = 0xff; MachEmptyWriteBuffer();
	vdac->over = 0xff; MachEmptyWriteBuffer();
	vdac->over = 0xff; MachEmptyWriteBuffer();
}

/*
 * ----------------------------------------------------------------------------
 *
 * pmLoadColorMap --
 *
 *	Load the color map.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The color map is loaded.
 *
 * ----------------------------------------------------------------------------
 */
static void
pmLoadColorMap(ptr)
	ColorMap *ptr;
{
	register VDACRegs *vdac = (VDACRegs *)MACH_PHYS_TO_UNCACHED(KN01_SYS_VDAC);

	if (ptr->index > 256)
		return;

	vdac->mapWA = ptr->index; MachEmptyWriteBuffer();
	vdac->map = ptr->Entry.red; MachEmptyWriteBuffer();
	vdac->map = ptr->Entry.green; MachEmptyWriteBuffer();
	vdac->map = ptr->Entry.blue; MachEmptyWriteBuffer();
}

/*
 *----------------------------------------------------------------------
 *
 * pmPosCursor --
 *
 *	Postion the cursor.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */
void
pmPosCursor(x, y)
	register int x, y;
{
	register PCCRegs *pcc = (PCCRegs *)MACH_PHYS_TO_UNCACHED(KN01_SYS_PCC);
	register struct pmax_fb *fp = &pmfb;

	if (y < fp->fbu->scrInfo.min_cur_y || y > fp->fbu->scrInfo.max_cur_y)
		y = fp->fbu->scrInfo.max_cur_y;
	if (x < fp->fbu->scrInfo.min_cur_x || x > fp->fbu->scrInfo.max_cur_x)
		x = fp->fbu->scrInfo.max_cur_x;
	fp->fbu->scrInfo.cursor.x = x;		/* keep track of real cursor */
	fp->fbu->scrInfo.cursor.y = y;		/* position, indep. of mouse */
	pcc->xpos = PCC_X_OFFSET + x;
	pcc->ypos = PCC_Y_OFFSET + y;
}

/*
 * pm keyboard and mouse input. Just punt to the generic ones in fb.c
 */
void
pmKbdEvent(ch)
	int ch;
{
	fbKbdEvent(ch, &pmfb);
}

void
pmMouseEvent(newRepPtr)
	MouseReport *newRepPtr;
{
	fbMouseEvent(newRepPtr, &pmfb);
}

void
pmMouseButtons(newRepPtr)
	MouseReport *newRepPtr;
{
	fbMouseButtons(newRepPtr, &pmfb);
}
#endif /* NDC */
#endif /* NPM */
