/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell and Rick Macklem.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)xcfb.c	7.2 (Berkeley) %G%
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

#include <xcfb.h>
#include <dtop.h>
#if NXCFB > 0
#if NDTOP == 0
xcfb needs dtop device
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
#include <machine/pmioctl.h>

#include <pmax/pmax/maxine.h>
#include <pmax/pmax/cons.h>
#include <pmax/pmax/pmaxtype.h>

#include <pmax/dev/device.h>
#include <pmax/dev/xcfbreg.h>
#include <pmax/dev/dtopreg.h>
#include <pmax/dev/fbreg.h>

/*
 * These need to be mapped into user space.
 */
struct fbuaccess xcfbu;
struct pmax_fb xcfbfb;

/*
 * Forward references.
 */
extern void fbScroll();

static void xcfbScreenInit();
static void xcfbLoadCursor();
static void xcfbRestoreCursorColor();
static void xcfbCursorColor();
void xcfbPosCursor();
static void xcfbInitColorMap();
static void xcfbLoadColorMap();
static u_int ims332_read_register();
static void ims332_write_register();
static void ims332_load_colormap_entry();
static void ims332_video_off();
static void ims332_video_on();

extern void dtopKBDPutc(), fbKbdEvent(), fbMouseEvent(), fbMouseButtons();
void xcfbKbdEvent(), xcfbMouseEvent(), xcfbMouseButtons();
extern void (*dtopDivertXInput)();
extern void (*dtopMouseEvent)();
extern void (*dtopMouseButtons)();
extern int pmax_boardtype;
extern u_short defCursor[32];
extern struct consdev cn_tab;

int	xcfbprobe();
struct	driver xcfbdriver = {
	"xcfb", xcfbprobe, 0, 0,
};

/*
 * Test to see if device is present.
 * Return true if found and initialized ok.
 */
/*ARGSUSED*/
xcfbprobe(cp)
	register struct pmax_ctlr *cp;
{
	register struct pmax_fb *fp = &xcfbfb;

	if (pmax_boardtype != DS_MAXINE)
		return (0);
	if (!fp->initialized && !xcfbinit())
		return (0);
	printf("xcfb0 (color display)\n");
	return (1);
}

/*ARGSUSED*/
xcfbopen(dev, flag)
	dev_t dev;
	int flag;
{
	register struct pmax_fb *fp = &xcfbfb;
	int s;

	if (!fp->initialized)
		return (ENXIO);
	if (fp->GraphicsOpen)
		return (EBUSY);

	fp->GraphicsOpen = 1;
	xcfbInitColorMap();
	/*
	 * Set up event queue for later
	 */
	fp->fbu->scrInfo.qe.eSize = PM_MAXEVQ;
	fp->fbu->scrInfo.qe.eHead = fp->fbu->scrInfo.qe.eTail = 0;
	fp->fbu->scrInfo.qe.tcSize = MOTION_BUFFER_SIZE;
	fp->fbu->scrInfo.qe.tcNext = 0;
	fp->fbu->scrInfo.qe.timestamp_ms = TO_MS(time);
	s = spltty();
	dtopDivertXInput = xcfbKbdEvent;
	dtopMouseEvent = xcfbMouseEvent;
	dtopMouseButtons = xcfbMouseButtons;
	splx(s);
	return (0);
}

/*ARGSUSED*/
xcfbclose(dev, flag)
	dev_t dev;
	int flag;
{
	register struct pmax_fb *fp = &xcfbfb;
	int s;

	if (!fp->GraphicsOpen)
		return (EBADF);

	fp->GraphicsOpen = 0;
	xcfbInitColorMap();
	s = spltty();
	dtopDivertXInput = (void (*)())0;
	dtopMouseEvent = (void (*)())0;
	dtopMouseButtons = (void (*)())0;
	splx(s);
	xcfbScreenInit();
	vmUserUnmap();
	bzero((caddr_t)fp->fr_addr, 1024 * 768);
	xcfbPosCursor(fp->col * 8, fp->row * 15);
	return (0);
}

/*ARGSUSED*/
xcfbioctl(dev, cmd, data, flag)
	dev_t dev;
	caddr_t data;
{
	register struct pmax_fb *fp = &xcfbfb;
	int s;

	switch (cmd) {
	case QIOCGINFO:
	    {
		caddr_t addr;
		extern caddr_t vmUserMap();

		/*
		 * Map the all the data the user needs access to into
		 * user space.
		 */
		addr = vmUserMap(sizeof(struct fbuaccess), (unsigned)fp->fbu);
		if (addr == (caddr_t)0)
			goto mapError;
		*(PM_Info **)data = &((struct fbuaccess *)addr)->scrInfo;
		fp->fbu->scrInfo.qe.events = ((struct fbuaccess *)addr)->events;
		fp->fbu->scrInfo.qe.tcs = ((struct fbuaccess *)addr)->tcs;
		fp->fbu->scrInfo.planemask = (char *)0;
		/*
		 * Map the frame buffer into the user's address space.
		 */
		addr = vmUserMap(1024 * 1024, (unsigned)fp->fr_addr);
		if (addr == (caddr_t)0)
			goto mapError;
		fp->fbu->scrInfo.bitmap = (char *)addr;
		break;

	mapError:
		vmUserUnmap();
		printf("Cannot map shared data structures\n");
		return (EIO);
	    }

	case QIOCPMSTATE:
		/*
		 * Set mouse state.
		 */
		fp->fbu->scrInfo.mouse = *(pmCursor *)data;
		xcfbPosCursor(fp->fbu->scrInfo.mouse.x, fp->fbu->scrInfo.mouse.y);
		break;

	case QIOCINIT:
		/*
		 * Initialize the screen.
		 */
		xcfbScreenInit();
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
	    }
	    break;

	case QIOCADDR:
		*(PM_Info **)data = &fp->fbu->scrInfo;
		break;

	case QIOWCURSOR:
		xcfbLoadCursor((unsigned short *)data);
		break;

	case QIOWCURSORCOLOR:
		xcfbCursorColor((unsigned int *)data);
		break;

	case QIOSETCMAP:
		xcfbLoadColorMap((ColorMap *)data);
		break;

	case QIOKERNLOOP:
		s = spltty();
		dtopDivertXInput = xcfbKbdEvent;
		dtopMouseEvent = xcfbMouseEvent;
		dtopMouseButtons = xcfbMouseButtons;
		splx(s);
		break;

	case QIOKERNUNLOOP:
		s = spltty();
		dtopDivertXInput = (void (*)())0;
		dtopMouseEvent = (void (*)())0;
		dtopMouseButtons = (void (*)())0;
		splx(s);
		break;

	case QIOVIDEOON:
		xcfbRestoreCursorColor();
		ims332_video_on();
		break;

	case QIOVIDEOOFF:
		ims332_video_off();
		break;

	default:
		printf("xcfb0: Unknown ioctl command %x\n", cmd);
		return (EINVAL);
	}
	return (0);
}

xcfbselect(dev, flag, p)
	dev_t dev;
	int flag;
	struct proc *p;
{
	struct pmax_fb *fp = &xcfbfb;

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
 *	Routines for the Inmos IMS-G332 Colour video controller
 * 	Author: Alessandro Forin, Carnegie Mellon University
 */
static u_int
ims332_read_register(regno)
{
	register u_char *regs = (u_char *)IMS332_ADDRESS;
	unsigned char *rptr;
	register u_int val, v1;

	/* spec sez: */
	rptr = regs + 0x80000 + (regno << 4);
	val = *((volatile u_short *) rptr );
	v1  = *((volatile u_short *) regs );

	return (val & 0xffff) | ((v1 & 0xff00) << 8);
}

static void
ims332_write_register(regno, val)
	register unsigned int val;
{
	register u_char *regs = (u_char *)IMS332_ADDRESS;
	u_char *wptr;

	/* spec sez: */
	wptr = regs + 0xa0000 + (regno << 4);
	*((volatile u_int *)(regs)) = (val >> 8) & 0xff00;
	*((volatile u_short *)(wptr)) = val;
}

#define	assert_ims332_reset_bit(r)	*r &= ~0x40
#define	deassert_ims332_reset_bit(r)	*r |=  0x40

/*
 * Color map
 */
static void
xcfbLoadColorMap(ptr)
	ColorMap *ptr;
{
	register int i;

	if (ptr->index > 256)
		return;
	ims332_load_colormap_entry(ptr->index, ptr);
}

static void
ims332_load_colormap_entry(entry, map)
	ColorMap *map;
{
	/* ?? stop VTG */
	ims332_write_register(IMS332_REG_LUT_BASE + (entry & 0xff),
			      (map->Entry.blue << 16) |
			      (map->Entry.green << 8) |
			      (map->Entry.red));
}

static void
xcfbInitColorMap()
{
	register int i;
	ColorMap m;

	m.Entry.red = m.Entry.green = m.Entry.blue = 0;
	ims332_load_colormap_entry(0, &m);

	m.Entry.red = m.Entry.green = m.Entry.blue = 0xff;
	for (i = 1; i < 256; i++)
		ims332_load_colormap_entry(i, &m);

	for (i = 0; i < 3; i++) {
		cursor_RGB[i] = 0x00;
		cursor_RGB[i + 3] = 0xff;
	}
	xcfbRestoreCursorColor();
}

/*
 * Video on/off
 *
 * It is unfortunate that X11 goes backward with white@0
 * and black@1.  So we must stash away the zero-th entry
 * and fix it while screen is off.  Also must remember
 * it, sigh.
 */
static struct {
	u_int	save;
	int	off;
} xcfb_vstate;

static void
ims332_video_off()
{
	register u_int csr;

	if (xcfb_vstate.off)
		return;

	xcfb_vstate.save = ims332_read_register(IMS332_REG_LUT_BASE);

	ims332_write_register(IMS332_REG_LUT_BASE, 0);

	ims332_write_register(IMS332_REG_COLOR_MASK, 0);

	/* cursor now */
	csr = ims332_read_register(IMS332_REG_CSR_A);
	csr |= IMS332_CSR_A_DISABLE_CURSOR;
	ims332_write_register(IMS332_REG_CSR_A, csr);

	xcfb_vstate.off = 1;
}

static void
ims332_video_on()
{
	register u_int csr;

	if (!xcfb_vstate.off)
		return;

	ims332_write_register(IMS332_REG_LUT_BASE, xcfb_vstate.save);

	ims332_write_register(IMS332_REG_COLOR_MASK, 0xffffffff);

	/* cursor now */
	csr = ims332_read_register(IMS332_REG_CSR_A);
	csr &= ~IMS332_CSR_A_DISABLE_CURSOR;
	ims332_write_register(IMS332_REG_CSR_A, csr);

	xcfb_vstate.off = 0;
}

/*
 * Cursor
 */
void
xcfbPosCursor(x, y)
	register int x, y;
{
	register struct pmax_fb *fp = &xcfbfb;

	if (y < fp->fbu->scrInfo.min_cur_y || y > fp->fbu->scrInfo.max_cur_y)
		y = fp->fbu->scrInfo.max_cur_y;
	if (x < fp->fbu->scrInfo.min_cur_x || x > fp->fbu->scrInfo.max_cur_x)
		x = fp->fbu->scrInfo.max_cur_x;
	fp->fbu->scrInfo.cursor.x = x;		/* keep track of real cursor */
	fp->fbu->scrInfo.cursor.y = y;		/* position, indep. of mouse */
	ims332_write_register(IMS332_REG_CURSOR_LOC,
		((x & 0xfff) << 12) | (y & 0xfff));
}

/*
 * xcfbRestoreCursorColor
 */
static void
xcfbRestoreCursorColor()
{

	/* Bg is color[0], Fg is color[1] */
	ims332_write_register(IMS332_REG_CURSOR_LUT_0,
			      (cursor_RGB[2] << 16) |
			      (cursor_RGB[1] << 8) |
			      (cursor_RGB[0]));
	ims332_write_register(IMS332_REG_CURSOR_LUT_1, 0x7f0000);
	ims332_write_register(IMS332_REG_CURSOR_LUT_2,
			      (cursor_RGB[5] << 16) |
			      (cursor_RGB[4] << 8) |
			      (cursor_RGB[3]));
}

/*
 * ----------------------------------------------------------------------------
 *
 * xcfbCursorColor --
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
xcfbCursorColor(color)
	unsigned int color[];
{
	register int i, j;

	for (i = 0; i < 6; i++)
		cursor_RGB[i] = (u_char)(color[i] >> 8);

	xcfbRestoreCursorColor();
}

static void
xcfbLoadCursor(cursor)
	u_short *cursor;
{
	register int i, j, k, pos;
	register u_short ap, bp, out;

	/*
	 * Fill in the cursor sprite using the A and B planes, as provided
	 * for the pmax.
	 * XXX This will have to change when the X server knows that this
	 * is not a pmax display.
	 */
	pos = 0;
	for (k = 0; k < 16; k++) {
		ap = *cursor;
		bp = *(cursor + 16);
		j = 0;
		while (j < 2) {
			out = 0;
			for (i = 0; i < 8; i++) {
				out = ((out >> 2) & 0x3fff) |
					((ap & 0x1) << 15) |
					((bp & 0x1) << 14);
				ap >>= 1;
				bp >>= 1;
			}
			ims332_write_register(IMS332_REG_CURSOR_RAM + pos, out);
			pos++;
			j++;
		}
		while (j < 8) {
			ims332_write_register(IMS332_REG_CURSOR_RAM + pos, 0);
			pos++;
			j++;
		}
		cursor++;
	}
	while (pos < 512) {
		ims332_write_register(IMS332_REG_CURSOR_RAM + pos, 0);
		pos++;
	}
}

/*
 * Initialization
 */
int
xcfbinit()
{
	register u_int *reset = (u_int *)IMS332_RESET_ADDRESS;
	register struct pmax_fb *fp = &xcfbfb;

	fp->isMono = 0;

	/*
	 * Or Cached? A comment in the Mach driver suggests that the X server
	 * runs faster in cached address space, but the X server is going
	 * to blow away the data cache whenever it updates the screen, so..
	 */
	fp->fr_addr = (char *)
		MACH_PHYS_TO_UNCACHED(XINE_PHYS_CFB_START + VRAM_OFFSET);

	/*
	 * Must be in Uncached space or the Xserver sees a stale version of
	 * the event queue and acts totally wacko. I don't understand this,
	 * since the R3000 uses a physical address cache?
	 */
	fp->fbu = (struct fbuaccess *)
		MACH_PHYS_TO_UNCACHED(MACH_CACHED_TO_PHYS(&xcfbu));
	fp->posCursor = xcfbPosCursor;
	fp->KBDPutc = dtopKBDPutc;
	fp->kbddev = makedev(DTOPDEV, DTOPKBD_PORT);

	/*
	 * Initialize the screen.
	 */
#ifdef notdef
	assert_ims332_reset_bit(reset);
	DELAY(1);	/* specs sez 50ns.. */
	deassert_ims332_reset_bit(reset);

	/* CLOCKIN appears to receive a 6.25 Mhz clock --> PLL 12 for 75Mhz monitor */
	ims332_write_register(IMS332_REG_BOOT, 12 | IMS332_BOOT_CLOCK_PLL);

	/* initialize VTG */
	ims332_write_register(IMS332_REG_CSR_A,
				IMS332_BPP_8 | IMS332_CSR_A_DISABLE_CURSOR);
	DELAY(50);	/* spec does not say */

	/* datapath registers (values taken from prom's settings) */

	ims332_write_register(IMS332_REG_HALF_SYNCH, 0x10);
	ims332_write_register(IMS332_REG_BACK_PORCH, 0x21);
	ims332_write_register(IMS332_REG_DISPLAY, 0x100);
	ims332_write_register(IMS332_REG_SHORT_DIS, 0x5d);
	ims332_write_register(IMS332_REG_BROAD_PULSE, 0x9f);
	ims332_write_register(IMS332_REG_V_SYNC, 0xc);
	ims332_write_register(IMS332_REG_V_PRE_EQUALIZE, 2);
	ims332_write_register(IMS332_REG_V_POST_EQUALIZE, 2);
	ims332_write_register(IMS332_REG_V_BLANK, 0x2a);
	ims332_write_register(IMS332_REG_V_DISPLAY, 0x600);
	ims332_write_register(IMS332_REG_LINE_TIME, 0x146);
	ims332_write_register(IMS332_REG_LINE_START, 0x10);
	ims332_write_register(IMS332_REG_MEM_INIT, 0xa);
	ims332_write_register(IMS332_REG_XFER_DELAY, 0xa);

	ims332_write_register(IMS332_REG_COLOR_MASK, 0xffffff);
#endif

	/*
	 * Initialize screen info.
	 */
	fp->fbu->scrInfo.max_row = 50;
	fp->fbu->scrInfo.max_col = 80;
	fp->fbu->scrInfo.max_x = 1024;
	fp->fbu->scrInfo.max_y = 768;
	fp->fbu->scrInfo.max_cur_x = 1008;
	fp->fbu->scrInfo.max_cur_y = 752;
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

	xcfbInitColorMap();

	ims332_write_register(IMS332_REG_CSR_A,
		IMS332_BPP_8 | IMS332_CSR_A_DMA_DISABLE | IMS332_CSR_A_VTG_ENABLE);

	xcfbScreenInit();
	fbScroll(fp);

	fp->initialized = 1;
	if (cn_tab.cn_fb == (struct pmax_fb *)0)
		cn_tab.cn_fb = fp;
	return (1);
}

/*
 * ----------------------------------------------------------------------------
 *
 * xcfbScreenInit --
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
xcfbScreenInit()
{
	register struct pmax_fb *fp = &xcfbfb;

	/*
	 * Home the cursor.
	 * We want an LSI terminal emulation.  We want the graphics
	 * terminal to scroll from the bottom. So start at the bottom.
	 */
	fp->row = 49;
	fp->col = 0;

	/*
	 * Load the cursor with the default values
	 *
	 */
	xcfbLoadCursor(defCursor);
}

/*
 * xcfb keyboard and mouse input. Just punt to the generic ones in fb.c
 */
void
xcfbKbdEvent(ch)
	int ch;
{
	fbKbdEvent(ch, &xcfbfb);
}

void
xcfbMouseEvent(newRepPtr)
	MouseReport *newRepPtr;
{
	fbMouseEvent(newRepPtr, &xcfbfb);
}

void
xcfbMouseButtons(newRepPtr)
	MouseReport *newRepPtr;
{
	fbMouseButtons(newRepPtr, &xcfbfb);
}
#endif /* NDTOP */
#endif /* NXCFB */
