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
 *	@(#)mfb.c	8.1 (Berkeley) 6/10/93
 */

/* 
 * Mach Operating System
 * Copyright (c) 1991,1990,1989 Carnegie Mellon University
 * All Rights Reserved.
 * 
 * Permission to use, copy, modify and distribute this software and its
 * documentation is hereby granted, provided that both the copyright
 * notice and this permission notice appear in all copies of the
 * software, derivative works or modified versions, and any portions
 * thereof, and that both notices appear in supporting documentation.
 * 
 * CARNEGIE MELLON ALLOWS FREE USE OF THIS SOFTWARE IN ITS 
 * CONDITION.  CARNEGIE MELLON DISCLAIMS ANY LIABILITY OF ANY KIND FOR
 * ANY DAMAGES WHATSOEVER RESULTING FROM THE USE OF THIS SOFTWARE.
 * 
 * Carnegie Mellon requests users of this software to return to
 * 
 *  Software Distribution Coordinator  or  Software.Distribution@CS.CMU.EDU
 *  School of Computer Science
 *  Carnegie Mellon University
 *  Pittsburgh PA 15213-3890
 * 
 * any improvements or extensions that they make and grant Carnegie the
 * rights to redistribute these changes.
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

#include <mfb.h>
#if NMFB > 0
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
#include <machine/pmioctl.h>

#include <pmax/pmax/cons.h>
#include <pmax/pmax/pmaxtype.h>

#include <pmax/dev/device.h>
#include <pmax/dev/mfbreg.h>
#include <pmax/dev/fbreg.h>

#include <dc.h>
#include <dtop.h>
#include <scc.h>

/*
 * These need to be mapped into user space.
 */
struct fbuaccess mfbu;
struct pmax_fb mfbfb;

/*
 * Forward references.
 */
static void mfbScreenInit();
static void mfbLoadCursor();
static void mfbRestoreCursorColor();
static void mfbCursorColor();
void mfbPosCursor();
static void mfbInitColorMap();
static void mfbLoadColorMap();
static void mfbConfigMouse(), mfbDeconfigMouse();
static void bt455_video_on(), bt455_video_off(), bt431_select_reg();
static void bt431_write_reg(), bt431_init();
static u_char bt431_read_reg();

void mfbKbdEvent(), mfbMouseEvent(), mfbMouseButtons();
#if NDC > 0
extern void (*dcDivertXInput)();
extern void (*dcMouseEvent)();
extern void (*dcMouseButtons)();
#endif
#if NSCC > 0
extern void (*sccDivertXInput)();
extern void (*sccMouseEvent)();
extern void (*sccMouseButtons)();
#endif
#if NDTOP > 0
extern void (*dtopDivertXInput)();
extern void (*dtopMouseEvent)();
extern void (*dtopMouseButtons)();
#endif
extern int pmax_boardtype;
extern u_short defCursor[32];
extern struct consdev cn_tab;

int	mfbprobe();
struct	driver mfbdriver = {
	"mfb", mfbprobe, 0, 0,
};

#define	MFB_OFFSET_VRAM		0x200000	/* from module's base */
#define MFB_OFFSET_BT431	0x180000	/* Bt431 registers */
#define MFB_OFFSET_BT455	0x100000	/* Bt455 registers */
#define MFB_OFFSET_IREQ		0x080000	/* Interrupt req. control */
#define MFB_OFFSET_ROM		0x0		/* Diagnostic ROM */
#define MFB_FB_SIZE		0x200000	/* frame buffer size */

/*
 * Test to see if device is present.
 * Return true if found and initialized ok.
 */
/*ARGSUSED*/
mfbprobe(cp)
	register struct pmax_ctlr *cp;
{
	register struct pmax_fb *fp = &mfbfb;

	if (!fp->initialized && !mfbinit(cp->pmax_addr))
		return (0);
	printf("mfb0 (mono display)\n");
	return (1);
}

/*ARGSUSED*/
mfbopen(dev, flag)
	dev_t dev;
	int flag;
{
	register struct pmax_fb *fp = &mfbfb;
	int s;

	if (!fp->initialized)
		return (ENXIO);
	if (fp->GraphicsOpen)
		return (EBUSY);

	fp->GraphicsOpen = 1;
	mfbInitColorMap(1);
	/*
	 * Set up event queue for later
	 */
	fp->fbu->scrInfo.qe.eSize = PM_MAXEVQ;
	fp->fbu->scrInfo.qe.eHead = fp->fbu->scrInfo.qe.eTail = 0;
	fp->fbu->scrInfo.qe.tcSize = MOTION_BUFFER_SIZE;
	fp->fbu->scrInfo.qe.tcNext = 0;
	fp->fbu->scrInfo.qe.timestamp_ms = TO_MS(time);
	mfbConfigMouse();
	return (0);
}

/*ARGSUSED*/
mfbclose(dev, flag)
	dev_t dev;
	int flag;
{
	register struct pmax_fb *fp = &mfbfb;
	int s;

	if (!fp->GraphicsOpen)
		return (EBADF);

	fp->GraphicsOpen = 0;
	mfbInitColorMap(0);
	mfbDeconfigMouse();
	mfbScreenInit();
	bzero((caddr_t)fp->fr_addr, 2048 * 1024);
	mfbPosCursor(fp->col * 8, fp->row * 15);
	return (0);
}

/*ARGSUSED*/
mfbioctl(dev, cmd, data, flag, p)
	dev_t dev;
	caddr_t data;
	struct proc *p;
{
	register struct pmax_fb *fp = &mfbfb;
	int s;

	switch (cmd) {
	case QIOCGINFO:
		return (fbmmap(fp, dev, data, p));

	case QIOCPMSTATE:
		/*
		 * Set mouse state.
		 */
		fp->fbu->scrInfo.mouse = *(pmCursor *)data;
		mfbPosCursor(fp->fbu->scrInfo.mouse.x, fp->fbu->scrInfo.mouse.y);
		break;

	case QIOCINIT:
		/*
		 * Initialize the screen.
		 */
		mfbScreenInit();
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
		mfbLoadCursor((unsigned short *)data);
		break;

	case QIOWCURSORCOLOR:
		mfbCursorColor((unsigned int *)data);
		break;

	case QIOSETCMAP:
#ifdef notdef
		mfbLoadColorMap((ColorMap *)data);
#endif
		break;

	case QIOKERNLOOP:
		mfbConfigMouse();
		break;

	case QIOKERNUNLOOP:
		mfbDeconfigMouse();
		break;

	case QIOVIDEOON:
		bt455_video_on();
		break;

	case QIOVIDEOOFF:
		bt455_video_off();
		break;

	default:
		printf("mfb0: Unknown ioctl command %x\n", cmd);
		return (EINVAL);
	}
	return (0);
}

/*
 * Return the physical page number that corresponds to byte offset 'off'.
 */
/*ARGSUSED*/
mfbmap(dev, off, prot)
	dev_t dev;
{
	int len;

	len = pmax_round_page(((vm_offset_t)&mfbu & PGOFSET) + sizeof(mfbu));
	if (off < len)
		return pmax_btop(MACH_CACHED_TO_PHYS(&mfbu) + off);
	off -= len;
	if (off >= mfbfb.fr_size)
		return (-1);
	return pmax_btop(MACH_UNCACHED_TO_PHYS(mfbfb.fr_addr) + off);
}

mfbselect(dev, flag, p)
	dev_t dev;
	int flag;
	struct proc *p;
{
	struct pmax_fb *fp = &mfbfb;

	switch (flag) {
	case FREAD:
		if (fp->fbu->scrInfo.qe.eHead != fp->fbu->scrInfo.qe.eTail)
			return (1);
		selrecord(p, &fp->selp);
		break;
	}

	return (0);
}

static u_char	cursor_RGB[6];	/* cursor color 2 & 3 */

/*
 * There are actually 2 Bt431 cursor sprite chips that each generate 1 bit
 * of each cursor pixel for a 2bit 64x64 cursor sprite. The corresponding
 * registers for these two chips live in adjacent bytes of the shorts that
 * are defined in bt431_regmap_t.
 */
static void
mfbLoadCursor(cursor)
	u_short *cursor;
{
	register int i, j, k, pos;
	register u_short ap, bp, out;
	register bt431_regmap_t *regs;

	regs = (bt431_regmap_t *)(mfbfb.fr_chipaddr +
		 MFB_OFFSET_BT431);
	/*
	 * Fill in the cursor sprite using the A and B planes, as provided
	 * for the pmax.
	 * XXX This will have to change when the X server knows that this
	 * is not a pmax display. (ie. Not the Xcfbpmax server.)
	 */
	pos = 0;
	bt431_select_reg(regs, BT431_REG_CRAM_BASE);
	for (k = 0; k < 16; k++) {
		ap = *cursor;
		bp = *(cursor + 16);
		j = 0;
		while (j < 2) {
			out = 0;
			for (i = 0; i < 8; i++) {
				out = (out << 1) | ((bp & 0x1) << 8) |
					(ap & 0x1);
				ap >>= 1;
				bp >>= 1;
			}
			BT431_WRITE_CMAP_AUTOI(regs, out);
			pos++;
			j++;
		}
		while (j < 8) {
			BT431_WRITE_CMAP_AUTOI(regs, 0);
			pos++;
			j++;
		}
		cursor++;
	}
	while (pos < 512) {
		BT431_WRITE_CMAP_AUTOI(regs, 0);
		pos++;
	}
}

/*
 * Initialization
 */
int
mfbinit(cp)
	char *cp;
{
	register struct pmax_fb *fp = &mfbfb;

	/* check for no frame buffer */
	if (badaddr(cp, 4))
		return (0);

	fp->isMono = 0;
	fp->fr_addr = cp + MFB_OFFSET_VRAM;
	fp->fr_chipaddr = cp;
	fp->fr_size = MFB_FB_SIZE;
	/*
	 * Must be in Uncached space since the fbuaccess structure is
	 * mapped into the user's address space uncached.
	 */
	fp->fbu = (struct fbuaccess *)
		MACH_PHYS_TO_UNCACHED(MACH_CACHED_TO_PHYS(&mfbu));
	fp->posCursor = mfbPosCursor;
	if (tb_kbdmouseconfig(fp))
		return (0);

	/*
	 * Initialize the screen.
	 */
	bt431_init(fp->fr_chipaddr + MFB_OFFSET_BT431);

	/*
	 * Initialize screen info.
	 */
	fp->fbu->scrInfo.max_row = 67;
	fp->fbu->scrInfo.max_col = 80;
	fp->fbu->scrInfo.max_x = 1280;
	fp->fbu->scrInfo.max_y = 1024;
	fp->fbu->scrInfo.max_cur_x = 1279;
	fp->fbu->scrInfo.max_cur_y = 1023;
	fp->fbu->scrInfo.version = 11;
	fp->fbu->scrInfo.mthreshold = 4;
	fp->fbu->scrInfo.mscale = 2;
	fp->fbu->scrInfo.min_cur_x = 0;
	fp->fbu->scrInfo.min_cur_y = 0;
	fp->fbu->scrInfo.qe.timestamp_ms = TO_MS(time);
	fp->fbu->scrInfo.qe.eSize = PM_MAXEVQ;
	fp->fbu->scrInfo.qe.eHead = fp->fbu->scrInfo.qe.eTail = 0;
	fp->fbu->scrInfo.qe.tcSize = MOTION_BUFFER_SIZE;
	fp->fbu->scrInfo.qe.tcNext = 0;

	/*
	 * Initialize the color map, the screen, and the mouse.
	 */
	mfbInitColorMap(0);
	mfbScreenInit();
	fbScroll(fp);

	fp->initialized = 1;
	if (cn_tab.cn_fb == (struct pmax_fb *)0)
		cn_tab.cn_fb = fp;
	return (1);
}

/*
 * ----------------------------------------------------------------------------
 *
 * mfbScreenInit --
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
mfbScreenInit()
{
	register struct pmax_fb *fp = &mfbfb;

	/*
	 * Home the cursor.
	 * We want an LSI terminal emulation.  We want the graphics
	 * terminal to scroll from the bottom. So start at the bottom.
	 */
	fp->row = 66;
	fp->col = 0;

	/*
	 * Load the cursor with the default values
	 *
	 */
	mfbLoadCursor(defCursor);
}

/*
 * ----------------------------------------------------------------------------
 *
 * RestoreCursorColor --
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
mfbRestoreCursorColor()
{
	bt455_regmap_t *regs = (bt455_regmap_t *)(mfbfb.fr_chipaddr +
		MFB_OFFSET_BT455);
	ColorMap cm;
	u_char fg;

	if (cursor_RGB[0] || cursor_RGB[1] || cursor_RGB[2])
		cm.Entry.red = cm.Entry.green = cm.Entry.blue = 0xffff;
	else
		cm.Entry.red = cm.Entry.green = cm.Entry.blue = 0;
	cm.index = 8;
	mfbLoadColorMap(&cm);
	cm.index = 9;
	mfbLoadColorMap(&cm);

	if (cursor_RGB[3] || cursor_RGB[4] || cursor_RGB[5])
		fg = 0xf;
	else
		fg = 0;
	regs->addr_ovly = fg;
	MachEmptyWriteBuffer();
	regs->addr_ovly = fg;
	MachEmptyWriteBuffer();
	regs->addr_ovly = fg;
	MachEmptyWriteBuffer();
}

/*
 * ----------------------------------------------------------------------------
 *
 * CursorColor --
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
mfbCursorColor(color)
	unsigned int color[];
{
	register int i, j;

	for (i = 0; i < 6; i++)
		cursor_RGB[i] = (u_char)(color[i] >> 8);

	mfbRestoreCursorColor();
}

/*
 *----------------------------------------------------------------------
 *
 * PosCursor --
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
mfbPosCursor(x, y)
	register int x, y;
{
	bt431_regmap_t *regs = (bt431_regmap_t *)(mfbfb.fr_chipaddr +
		 MFB_OFFSET_BT431);
	register struct pmax_fb *fp = &mfbfb;

	if (y < fp->fbu->scrInfo.min_cur_y || y > fp->fbu->scrInfo.max_cur_y)
		y = fp->fbu->scrInfo.max_cur_y;
	if (x < fp->fbu->scrInfo.min_cur_x || x > fp->fbu->scrInfo.max_cur_x)
		x = fp->fbu->scrInfo.max_cur_x;
	fp->fbu->scrInfo.cursor.x = x;		/* keep track of real cursor */
	fp->fbu->scrInfo.cursor.y = y;		/* position, indep. of mouse */

#define lo(v)	((v)&0xff)
#define hi(v)	(((v)&0xf00)>>8)

	/*
	 * Cx = x + D + H - P
	 *  P = 37 if 1:1, 52 if 4:1, 57 if 5:1
	 *  D = pixel skew between outdata and external data
	 *  H = pixels between HSYNCH falling and active video
	 *
	 * Cy = y + V - 32
	 *  V = scanlines between HSYNCH falling, two or more
	 *	clocks after VSYNCH falling, and active video
	 */

	bt431_write_reg(regs, BT431_REG_CXLO, lo(x + 360));
	BT431_WRITE_REG_AUTOI(regs, hi(x + 360));
	BT431_WRITE_REG_AUTOI(regs, lo(y + 36));
	BT431_WRITE_REG_AUTOI(regs, hi(y + 36));
}

/*
 * ----------------------------------------------------------------------------
 *
 * InitColorMap --
 *
 *	Initialize the color map.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The colormap is initialized appropriately.
 *
 * ----------------------------------------------------------------------------
 */
static void
mfbInitColorMap(blackpix)
	int blackpix;
{
	ColorMap cm;
	register int i;

	cm.index = 0;
	if (blackpix)
		cm.Entry.red = cm.Entry.green = cm.Entry.blue = 0xffff;
	else
		cm.Entry.red = cm.Entry.green = cm.Entry.blue = 0;
	mfbLoadColorMap(&cm);
	if (blackpix)
		cm.Entry.red = cm.Entry.green = cm.Entry.blue = 0;
	else
		cm.Entry.red = cm.Entry.green = cm.Entry.blue = 0xffff;
	for (i = 1; i < 16; i++) {
		cm.index = i;
		mfbLoadColorMap(&cm);
	}

	for (i = 0; i < 3; i++) {
		cursor_RGB[i] = 0;
		cursor_RGB[i + 3] = 0xff;
	}
	mfbRestoreCursorColor();
}

/*
 * ----------------------------------------------------------------------------
 *
 * LoadColorMap --
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
mfbLoadColorMap(ptr)
	ColorMap *ptr;
{
	bt455_regmap_t *regs = (bt455_regmap_t *)(mfbfb.fr_chipaddr +
		 MFB_OFFSET_BT455);

	if (ptr->index > 15)
		return;

	BT455_SELECT_ENTRY(regs, ptr->index);
	regs->addr_cmap_data = ptr->Entry.red >> 12;
	MachEmptyWriteBuffer();
	regs->addr_cmap_data = ptr->Entry.green >> 12;
	MachEmptyWriteBuffer();
	regs->addr_cmap_data = ptr->Entry.blue >> 12;
	MachEmptyWriteBuffer();
}

/*
 * Video on/off state.
 */
static struct vstate {
	u_char	color0[6];	/* saved color map entry zero */
	u_char	off;		/* TRUE if display is off */
	u_char	cursor[6];	/* saved cursor color */
} vstate;

/*
 * ----------------------------------------------------------------------------
 *
 * bt455_video_on
 *
 *	Enable the video display.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The display is enabled.
 *
 * ----------------------------------------------------------------------------
 */
static void
bt455_video_on()
{
	register int i;
	bt455_regmap_t *regs = (bt455_regmap_t *)(mfbfb.fr_chipaddr +
		 MFB_OFFSET_BT455);

	if (!vstate.off)
		return;

	/* restore old color map entry zero */
	BT455_SELECT_ENTRY(regs, 0);
	for (i = 0; i < 6; i++) {
		regs->addr_cmap_data = vstate.color0[i];
		MachEmptyWriteBuffer();
		cursor_RGB[i] = vstate.cursor[i];
	}
	mfbRestoreCursorColor();
	vstate.off = 0;
}

/*
 * ----------------------------------------------------------------------------
 *
 * bt455_video_off
 *
 *	Disable the video display.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The display is disabled.
 *
 * ----------------------------------------------------------------------------
 */
static void
bt455_video_off()
{
	register int i;
	bt455_regmap_t *regs = (bt455_regmap_t *)(mfbfb.fr_chipaddr +
		 MFB_OFFSET_BT455);
	ColorMap cm;

	if (vstate.off)
		return;

	/* save old color map entry zero */
	BT455_SELECT_ENTRY(regs, 0);
	for (i = 0; i < 6; i++) {
		vstate.color0[i] = regs->addr_cmap_data;
		vstate.cursor[i] = cursor_RGB[i];
		cursor_RGB[i] = 0;
	}

	/* set color map entry zero to zero */
	cm.index = 0;
	cm.Entry.red = cm.Entry.green = cm.Entry.blue = 0;
	mfbLoadColorMap(&cm);
	cm.index = 1;
	mfbLoadColorMap(&cm);

	mfbRestoreCursorColor();
	vstate.off = 1;
}

/*
 * mfb keyboard and mouse input. Just punt to the generic ones in fb.c
 */
void
mfbKbdEvent(ch)
	int ch;
{
	fbKbdEvent(ch, &mfbfb);
}

void
mfbMouseEvent(newRepPtr)
	MouseReport *newRepPtr;
{
	fbMouseEvent(newRepPtr, &mfbfb);
}

void
mfbMouseButtons(newRepPtr)
	MouseReport *newRepPtr;
{
	fbMouseButtons(newRepPtr, &mfbfb);
}

/*
 * Configure the mouse and keyboard based on machine type
 */
static void
mfbConfigMouse()
{
	int s;

	s = spltty();
	switch (pmax_boardtype) {
#if NDC > 0
	case DS_3MAX:
		dcDivertXInput = mfbKbdEvent;
		dcMouseEvent = mfbMouseEvent;
		dcMouseButtons = mfbMouseButtons;
		break;
#endif
#if NSCC > 1
	case DS_3MIN:
		sccDivertXInput = mfbKbdEvent;
		sccMouseEvent = mfbMouseEvent;
		sccMouseButtons = mfbMouseButtons;
		break;
#endif
#if NDTOP > 0
	case DS_MAXINE:
		dtopDivertXInput = mfbKbdEvent;
		dtopMouseEvent = mfbMouseEvent;
		dtopMouseButtons = mfbMouseButtons;
		break;
#endif
	default:
		printf("Can't configure mouse/keyboard\n");
	};
	splx(s);
}

/*
 * and deconfigure them
 */
static void
mfbDeconfigMouse()
{
	int s;

	s = spltty();
	switch (pmax_boardtype) {
#if NDC > 0
	case DS_3MAX:
		dcDivertXInput = (void (*)())0;
		dcMouseEvent = (void (*)())0;
		dcMouseButtons = (void (*)())0;
		break;
#endif
#if NSCC > 1
	case DS_3MIN:
		sccDivertXInput = (void (*)())0;
		sccMouseEvent = (void (*)())0;
		sccMouseButtons = (void (*)())0;
		break;
#endif
#if NDTOP > 0
	case DS_MAXINE:
		dtopDivertXInput = (void (*)())0;
		dtopMouseEvent = (void (*)())0;
		dtopMouseButtons = (void (*)())0;
		break;
#endif
	default:
		printf("Can't deconfigure mouse/keyboard\n");
	};
}

/*
 * Generic register access
 */
static void
bt431_select_reg(regs, regno)
	bt431_regmap_t *regs;
{
	regs->addr_lo = SET_VALUE(regno & 0xff);
	regs->addr_hi = SET_VALUE((regno >> 8) & 0xff);
	MachEmptyWriteBuffer();
}

static void 
bt431_write_reg(regs, regno, val)
	bt431_regmap_t *regs;
{
	bt431_select_reg(regs, regno);
	regs->addr_reg = SET_VALUE(val);
	MachEmptyWriteBuffer();
}

static u_char
bt431_read_reg(regs, regno)
	bt431_regmap_t *regs;
{
	bt431_select_reg(regs, regno);
	return (GET_VALUE(regs->addr_reg));
}

static void
bt431_init(regs)
	bt431_regmap_t *regs;
{
	register int i;

	/* use 4:1 input mux */
	bt431_write_reg(regs, BT431_REG_CMD,
			 BT431_CMD_CURS_ENABLE|BT431_CMD_OR_CURSORS|
			 BT431_CMD_4_1_MUX|BT431_CMD_THICK_1);

	/* home cursor */
	BT431_WRITE_REG_AUTOI(regs, 0x00);
	BT431_WRITE_REG_AUTOI(regs, 0x00);
	BT431_WRITE_REG_AUTOI(regs, 0x00);
	BT431_WRITE_REG_AUTOI(regs, 0x00);

	/* no crosshair window */
	BT431_WRITE_REG_AUTOI(regs, 0x00);
	BT431_WRITE_REG_AUTOI(regs, 0x00);
	BT431_WRITE_REG_AUTOI(regs, 0x00);
	BT431_WRITE_REG_AUTOI(regs, 0x00);
	BT431_WRITE_REG_AUTOI(regs, 0x00);
	BT431_WRITE_REG_AUTOI(regs, 0x00);
	BT431_WRITE_REG_AUTOI(regs, 0x00);
	BT431_WRITE_REG_AUTOI(regs, 0x00);
}
#endif /* NMFB */
