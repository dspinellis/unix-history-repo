/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)cfb.c	7.4 (Berkeley) %G%
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

#include "cfb.h"
#if NCFB > 0

/*
 * This is a device driver for the PMAG-BA color frame buffer
 * on the TURBOchannel.
 * XXX This is just to get a console working;
 *	it will need changes to work with X11R5.
 */

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
#include <machine/machMon.h>
#include <machine/dc7085cons.h>
#include <machine/pmioctl.h>

#include <pmax/dev/device.h>
#include <pmax/dev/cfbreg.h>
#include <pmax/dev/font.c>

#define MAX_ROW	56
#define MAX_COL	80

/*
 * Macro to translate from a time struct to milliseconds.
 */
#define TO_MS(tv) ((tv.tv_sec * 1000) + (tv.tv_usec / 1000))

static int	isMono;		/* true if B&W frame buffer */
static int	initialized;	/* true if 'probe' was successful */
static int	GraphicsOpen;	/* true if the graphics device is open */
static int	row, col;	/* row and col for console cursor */
static struct	selinfo cfb_selp;	/* process waiting for select */
static unsigned	fb_addr;	/* frame buffer kernel virtual address */
static unsigned	planemask_addr;	/* plane mask kernel virtual address */

/*
 * These need to be mapped into user space.
 */
static struct pmuaccess {
	PM_Info		scrInfo;
	pmEvent		events[PM_MAXEVQ];	
	pmTimeCoord	tcs[MOTION_BUFFER_SIZE];
} pmu;

/*
 * Font mask bits used by Blitc().
 */
static unsigned int fontmaskBits[16] = {
	0x00000000,
	0x00000001,
	0x00000100,
	0x00000101,
	0x00010000,
	0x00010001,
	0x00010100,
	0x00010101,
	0x01000000,
	0x01000001,
	0x01000100,
	0x01000101,
	0x01010000,
	0x01010001,
	0x01010100,
	0x01010101
};

/*
 * Forward references.
 */
static void Scroll();
static void Blitc();

static void ScreenInit();
static void LoadCursor();
static void RestoreCursorColor();
static void CursorColor();
static void PosCursor();
static void InitColorMap();
static void LoadColorMap();
static void EnableVideo();
static void DisableVideo();

extern void dcKBDPutc();
extern void (*dcDivertXInput)();
extern void (*dcMouseEvent)();
extern void (*dcMouseButtons)();

int	cfbprobe();
struct	driver cfbdriver = {
	"cfb", cfbprobe, 0, 0,
};

/*
 * Test to see if device is present.
 * Return true if found and initialized ok.
 */
cfbprobe(cp)
	register struct pmax_ctlr *cp;
{

	if (!initialized) {
		if (!cfb_init(cp))
			return (0);
	}
	printf("cfb%d at nexus0 csr 0x%x priority %d\n",
		cp->pmax_unit, cp->pmax_addr, cp->pmax_pri);
	return (1);
}

/*
 *----------------------------------------------------------------------
 *
 * cfbKbdEvent --
 *
 *	Process a received character.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Events added to the queue.
 *
 *----------------------------------------------------------------------
 */
void
cfbKbdEvent(ch)
	int ch;
{
	register pmEvent *eventPtr;
	int i;

	if (!GraphicsOpen)
		return;

	/*
	 * See if there is room in the queue.
	 */
	i = PM_EVROUND(pmu.scrInfo.qe.eTail + 1);
	if (i == pmu.scrInfo.qe.eHead)
		return;

	/*
	 * Add the event to the queue.
	 */
	eventPtr = &pmu.events[pmu.scrInfo.qe.eTail];
	eventPtr->type = BUTTON_RAW_TYPE;
	eventPtr->device = KEYBOARD_DEVICE;
	eventPtr->x = pmu.scrInfo.mouse.x;
	eventPtr->y = pmu.scrInfo.mouse.y;
	eventPtr->time = TO_MS(time);
	eventPtr->key = ch;
	pmu.scrInfo.qe.eTail = i;
	selwakeup(&cfb_selp);
}

/*
 *----------------------------------------------------------------------
 *
 * cfbMouseEvent --
 *
 *	Process a mouse event.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	An event is added to the event queue.
 *
 *----------------------------------------------------------------------
 */
void
cfbMouseEvent(newRepPtr) 
	register MouseReport *newRepPtr;
{
	unsigned milliSec;
	int i;
	pmEvent *eventPtr;

	if (!GraphicsOpen)
		return;

	milliSec = TO_MS(time);

	/*
	 * Check to see if we have to accelerate the mouse
	 */
	if (pmu.scrInfo.mscale >= 0) {
		if (newRepPtr->dx >= pmu.scrInfo.mthreshold) {
			newRepPtr->dx +=
				(newRepPtr->dx - pmu.scrInfo.mthreshold) *
				pmu.scrInfo.mscale;
		}
		if (newRepPtr->dy >= pmu.scrInfo.mthreshold) {
			newRepPtr->dy +=
				(newRepPtr->dy - pmu.scrInfo.mthreshold) *
				pmu.scrInfo.mscale;
		}
	}

	/*
	 * Update mouse position
	 */
	if (newRepPtr->state & MOUSE_X_SIGN) {
		pmu.scrInfo.mouse.x += newRepPtr->dx;
		if (pmu.scrInfo.mouse.x > pmu.scrInfo.max_cur_x)
			pmu.scrInfo.mouse.x = pmu.scrInfo.max_cur_x;
	} else {
		pmu.scrInfo.mouse.x -= newRepPtr->dx;
		if (pmu.scrInfo.mouse.x < pmu.scrInfo.min_cur_x)
			pmu.scrInfo.mouse.x = pmu.scrInfo.min_cur_x;
	}
	if (newRepPtr->state & MOUSE_Y_SIGN) {
		pmu.scrInfo.mouse.y -= newRepPtr->dy;
		if (pmu.scrInfo.mouse.y < pmu.scrInfo.min_cur_y)
			pmu.scrInfo.mouse.y = pmu.scrInfo.min_cur_y;
	} else {
		pmu.scrInfo.mouse.y += newRepPtr->dy;
		if (pmu.scrInfo.mouse.y > pmu.scrInfo.max_cur_y)
			pmu.scrInfo.mouse.y = pmu.scrInfo.max_cur_y;
	}

	/*
	 * Move the hardware cursor.
	 */
	PosCursor(pmu.scrInfo.mouse.x, pmu.scrInfo.mouse.y);

	/*
	 * Store the motion event in the motion buffer.
	 */
	pmu.tcs[pmu.scrInfo.qe.tcNext].time = milliSec;
	pmu.tcs[pmu.scrInfo.qe.tcNext].x = pmu.scrInfo.mouse.x;
	pmu.tcs[pmu.scrInfo.qe.tcNext].y = pmu.scrInfo.mouse.y;
	if (++pmu.scrInfo.qe.tcNext >= MOTION_BUFFER_SIZE)
		pmu.scrInfo.qe.tcNext = 0;
	if (pmu.scrInfo.mouse.y < pmu.scrInfo.mbox.bottom &&
	    pmu.scrInfo.mouse.y >=  pmu.scrInfo.mbox.top &&
	    pmu.scrInfo.mouse.x < pmu.scrInfo.mbox.right &&
	    pmu.scrInfo.mouse.x >=  pmu.scrInfo.mbox.left)
		return;

	pmu.scrInfo.mbox.bottom = 0;
	if (PM_EVROUND(pmu.scrInfo.qe.eTail + 1) == pmu.scrInfo.qe.eHead)
		return;

	i = PM_EVROUND(pmu.scrInfo.qe.eTail - 1);
	if ((pmu.scrInfo.qe.eTail != pmu.scrInfo.qe.eHead) && 
	    (i != pmu.scrInfo.qe.eHead)) {
		pmEvent *eventPtr;

		eventPtr = &pmu.events[i];
		if (eventPtr->type == MOTION_TYPE) {
			eventPtr->x = pmu.scrInfo.mouse.x;
			eventPtr->y = pmu.scrInfo.mouse.y;
			eventPtr->time = milliSec;
			eventPtr->device = MOUSE_DEVICE;
			return;
		}
	}
	/*
	 * Put event into queue and wakeup any waiters.
	 */
	eventPtr = &pmu.events[pmu.scrInfo.qe.eTail];
	eventPtr->type = MOTION_TYPE;
	eventPtr->time = milliSec;
	eventPtr->x = pmu.scrInfo.mouse.x;
	eventPtr->y = pmu.scrInfo.mouse.y;
	eventPtr->device = MOUSE_DEVICE;
	pmu.scrInfo.qe.eTail = PM_EVROUND(pmu.scrInfo.qe.eTail + 1);
	selwakeup(&cfb_selp);
}

/*
 *----------------------------------------------------------------------
 *
 * cfbMouseButtons --
 *
 *	Process mouse buttons.
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
cfbMouseButtons(newRepPtr)
	MouseReport *newRepPtr;
{
	static char temp, oldSwitch, newSwitch;
	int i, j;
	pmEvent *eventPtr;
	static MouseReport lastRep;

	if (!GraphicsOpen)
		return;

	newSwitch = newRepPtr->state & 0x07;
	oldSwitch = lastRep.state & 0x07;

	temp = oldSwitch ^ newSwitch;
	if (temp == 0)
		return;
	for (j = 1; j < 8; j <<= 1) {
		if ((j & temp) == 0)
			continue;

		/*
		 * Check for room in the queue
		 */
		i = PM_EVROUND(pmu.scrInfo.qe.eTail+1);
		if (i == pmu.scrInfo.qe.eHead)
			return;

		/*
		 * Put event into queue.
		 */
		eventPtr = &pmu.events[pmu.scrInfo.qe.eTail];

		switch (j) {
		case RIGHT_BUTTON:
			eventPtr->key = EVENT_RIGHT_BUTTON;
			break;

		case MIDDLE_BUTTON:
			eventPtr->key = EVENT_MIDDLE_BUTTON;
			break;

		case LEFT_BUTTON:
			eventPtr->key = EVENT_LEFT_BUTTON;
		}
		if (newSwitch & j)
			eventPtr->type = BUTTON_DOWN_TYPE;
		else
			eventPtr->type = BUTTON_UP_TYPE;
		eventPtr->device = MOUSE_DEVICE;

		eventPtr->time = TO_MS(time);
		eventPtr->x = pmu.scrInfo.mouse.x;
		eventPtr->y = pmu.scrInfo.mouse.y;
	}
	pmu.scrInfo.qe.eTail = i;
	selwakeup(&cfb_selp);

	lastRep = *newRepPtr;
	pmu.scrInfo.mswitches = newSwitch;
}

/*
 *----------------------------------------------------------------------
 *
 * Scroll --
 *
 *	Scroll the screen.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static void
Scroll()
{
	register int *dest, *src;
	register int *end;
	register int temp0, temp1, temp2, temp3;
	register int i, scanInc, lineCount;
	int line;

	/*
	 * If the mouse is on we don't scroll so that the bit map remains sane.
	 */
	if (GraphicsOpen) {
		row = 0;
		return;
	}

	/*
	 *  The following is an optimization to cause the scrolling 
	 *  of text to be memory limited.  Basically the writebuffer is 
	 *  4 words (32 bits ea.) long so to achieve maximum speed we 
	 *  read and write in multiples of 4 words. We also limit the 
	 *  size to be MAX_COL characters for more speed. 
	 */
	if (isMono) {
		lineCount = 5;
		line = 1920 * 2;
		scanInc = 44;
	} else {
		lineCount = 40;
		scanInc = 96;
		line = 1920 * 8;
	}
	src = (int *)(fb_addr + line);
	dest = (int *)(fb_addr);
	end = (int *)(fb_addr + (60 * line) - line);
	do {
		i = 0;
		do {
			temp0 = src[0];
			temp1 = src[1];
			temp2 = src[2];
			temp3 = src[3];
			dest[0] = temp0;
			dest[1] = temp1;
			dest[2] = temp2;
			dest[3] = temp3;
			dest += 4;
			src += 4;
			i++;
		} while (i < lineCount);
		src += scanInc;
		dest += scanInc;
	} while (src < end);

	/* 
	 * Now zero out the last two lines 
	 */
	bzero(fb_addr + (row * line), 3 * line);
}

/*
 *----------------------------------------------------------------------
 *
 * cfbPutc --
 *
 *	Write a character to the console.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */
cfbPutc(c)
	register int c;
{
	int s;

	s = splhigh();	/* in case we do any printf's at interrupt time */
	if (initialized) {
#ifdef DEBUG
		/*
		 * If the HELP key is pressed, wait for another
		 * HELP key press to start/stop output.
		 */
		if (dcDebugGetc() == LK_HELP) {
			while (dcDebugGetc() != LK_HELP)
				;
		}
#endif
		Blitc(c);
	} else {
		void (*f)() = (void (*)())MACH_MON_PUTCHAR;

		(*f)(c);
	}
	splx(s);
}

/*
 *----------------------------------------------------------------------
 *
 * Blitc --
 *
 *	Write a character to the screen.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static void
Blitc(c)
	register int c;
{
	register char *bRow, *fRow;
	register int i;
	register int ote = isMono ? 256 : 1024; /* offset to table entry */
	int colMult = isMono ? 1 : 8;

	c &= 0xff;

	switch (c) {
	case '\t':
		for (i = 8 - (col & 0x7); i > 0; i--)
			Blitc(' ');
		break;

	case '\r':
		col = 0;
		break;

	case '\b':
		col--;
		if (col < 0)
			col = 0;
		break;

	case '\n':
		if (row + 1 >= MAX_ROW)
			Scroll();
		else
			row++;
		col = 0;
		break;

	case '\007':
		dcKBDPutc(LK_RING_BELL);
		break;

	default:
		/*
		 * 0xA1 to 0xFD are the printable characters added with 8-bit
		 * support.
		 */
		if (c < ' ' || c > '~' && c < 0xA1 || c > 0xFD)
			break;
		/*
		 * If the next character will wrap around then 
		 * increment row counter or scroll screen.
		 */
		if (col >= MAX_COL) {
			col = 0;
			if (row + 1 >= MAX_ROW)
				Scroll();
			else
				row++;
		}
		bRow = (char *)(fb_addr +
			(row * 15 & 0x3ff) * ote + col * colMult);
		i = c - ' ';
		/*
		 * This is to skip the (32) 8-bit 
		 * control chars, as well as DEL 
		 * and 0xA0 which aren't printable
		 */
		if (c > '~')
			i -= 34; 
		i *= 15;
		fRow = (char *)((int)pmFont + i);

		/* inline expansion for speed */
		if (isMono) {
			*bRow = *fRow++; bRow += ote;
			*bRow = *fRow++; bRow += ote;
			*bRow = *fRow++; bRow += ote;
			*bRow = *fRow++; bRow += ote;
			*bRow = *fRow++; bRow += ote;
			*bRow = *fRow++; bRow += ote;
			*bRow = *fRow++; bRow += ote;
			*bRow = *fRow++; bRow += ote;
			*bRow = *fRow++; bRow += ote;
			*bRow = *fRow++; bRow += ote;
			*bRow = *fRow++; bRow += ote;
			*bRow = *fRow++; bRow += ote;
			*bRow = *fRow++; bRow += ote;
			*bRow = *fRow++; bRow += ote;
			*bRow = *fRow++; bRow += ote;
		} else {
			register int j;
			register unsigned int *pInt;

			pInt = (unsigned int *)bRow;
			for (j = 0; j < 15; j++) {
				/*
				 * fontmaskBits converts a nibble
				 * (4 bytes) to a long word 
				 * containing 4 pixels corresponding
				 * to each bit in the nibble.  Thus
				 * we write two longwords for each
				 * byte in font.
				 * 
				 * Remember the font is 8 bits wide
				 * and 15 bits high.
				 *
				 * We add 256 to the pointer to
				 * point to the pixel on the 
				 * next scan line
				 * directly below the current
				 * pixel.
				 */
				pInt[0] = fontmaskBits[(*fRow) & 0xf];
				pInt[1] = fontmaskBits[((*fRow) >> 4) & 0xf];
				fRow++; 
				pInt += 256;
			}
		}
		col++; /* increment column counter */
	}
	if (!GraphicsOpen)
		PosCursor(col * 8, row * 15);
}

/*ARGSUSED*/
cfbopen(dev, flag)
	dev_t dev;
	int flag;
{

	if (!initialized)
		return (ENXIO);
	if (GraphicsOpen)
		return (EBUSY);

	GraphicsOpen = 1;
	if (!isMono)
		InitColorMap();
	/*
	 * Set up event queue for later
	 */
	pmu.scrInfo.qe.eSize = PM_MAXEVQ;
	pmu.scrInfo.qe.eHead = pmu.scrInfo.qe.eTail = 0;
	pmu.scrInfo.qe.tcSize = MOTION_BUFFER_SIZE;
	pmu.scrInfo.qe.tcNext = 0;
	pmu.scrInfo.qe.timestamp_ms = TO_MS(time);
	return (0);
}

/*ARGSUSED*/
cfbclose(dev, flag)
	dev_t dev;
	int flag;
{
	int s;

	if (!GraphicsOpen)
		return (EBADF);

	GraphicsOpen = 0;
	if (!isMono)
		InitColorMap();
	s = spltty();
	dcDivertXInput = (void (*)())0;
	dcMouseEvent = (void (*)())0;
	dcMouseButtons = (void (*)())0;
	splx(s);
	ScreenInit();
	vmUserUnmap();
	bzero(fb_addr, (isMono ? 1024 / 8 : 1024) * 864);
	PosCursor(col * 8, row * 15);
	return (0);
}

/*ARGSUSED*/
cfbioctl(dev, cmd, data, flag)
	dev_t dev;
	caddr_t data;
{
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
		addr = vmUserMap(sizeof(pmu), (unsigned)&pmu);
		if (addr == (caddr_t)0)
			goto mapError;
		*(PM_Info **)data = &((struct pmuaccess *)addr)->scrInfo;
		pmu.scrInfo.qe.events = ((struct pmuaccess *)addr)->events;
		pmu.scrInfo.qe.tcs = ((struct pmuaccess *)addr)->tcs;
		/*
		 * Map the plane mask into the user's address space.
		 */
		addr = vmUserMap(4, planemask_addr);
		if (addr == (caddr_t)0)
			goto mapError;
		pmu.scrInfo.planemask = (char *)addr;
		/*
		 * Map the frame buffer into the user's address space.
		 */
		addr = vmUserMap(isMono ? 256*1024 : 1024*1024, fb_addr);
		if (addr == (caddr_t)0)
			goto mapError;
		pmu.scrInfo.bitmap = (char *)addr;
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
		pmu.scrInfo.mouse = *(pmCursor *)data;
		PosCursor(pmu.scrInfo.mouse.x, pmu.scrInfo.mouse.y);
		break;

	case QIOCINIT:
		/*
		 * Initialize the screen.
		 */
		ScreenInit();
		break;

	case QIOCKPCMD:
	    {
		pmKpCmd *kpCmdPtr;
		unsigned char *cp;

		kpCmdPtr = (pmKpCmd *)data;
		if (kpCmdPtr->nbytes == 0)
			kpCmdPtr->cmd |= 0x80;
		if (!GraphicsOpen)
			kpCmdPtr->cmd |= 1;
		dcKBDPutc((int)kpCmdPtr->cmd);
		cp = &kpCmdPtr->par[0];
		for (; kpCmdPtr->nbytes > 0; cp++, kpCmdPtr->nbytes--) {
			if (kpCmdPtr->nbytes == 1)
				*cp |= 0x80;
			dcKBDPutc((int)*cp);
		}
		break;
	    }

	case QIOCADDR:
		*(PM_Info **)data = &pmu.scrInfo;
		break;

	case QIOWCURSOR:
		LoadCursor((unsigned short *)data);
		break;

	case QIOWCURSORCOLOR:
		CursorColor((unsigned int *)data);
		break;

	case QIOSETCMAP:
		LoadColorMap((ColorMap *)data);
		break;

	case QIOKERNLOOP:
		s = spltty();
		dcDivertXInput = cfbKbdEvent;
		dcMouseEvent = cfbMouseEvent;
		dcMouseButtons = cfbMouseButtons;
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
		EnableVideo();
		break;

	case QIOVIDEOOFF:
		DisableVideo();
		break;

	default:
		printf("cfb0: Unknown ioctl command %x\n", cmd);
		return (EINVAL);
	}
	return (0);
}

cfbselect(dev, flag, p)
	dev_t dev;
	int flag;
	struct proc *p;
{

	switch (flag) {
	case FREAD:
		if (pmu.scrInfo.qe.eHead != pmu.scrInfo.qe.eTail)
			return (1);
		selrecord(p, &cfb_selp);
		break;
	}

	return (0);
}

static u_char	cursor_RGB[6];	/* cursor color 2 & 3 */

/*
 * The default cursor.
 */
static unsigned short defCursor[1024] = {
	0x00FF, 0x00FF, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x00FF, 0x00FF, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x00FF, 0x00FF, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x00FF, 0x00FF, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x00FF, 0x00FF, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x00FF, 0x00FF, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x00FF, 0x00FF, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x00FF, 0x00FF, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x00FF, 0x00FF, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x00FF, 0x00FF, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x00FF, 0x00FF, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x00FF, 0x00FF, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x00FF, 0x00FF, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x00FF, 0x00FF, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x00FF, 0x00FF, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x00FF, 0x00FF, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
	0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
};

#define	CFB_OFFSET_VRAM		0x0		/* from module's base */
						/* Replicated at x100000 */
#define CFB_OFFSET_BT459	0x200000	/* Bt459 registers */
#define CFB_OFFSET_IREQ		0x300000	/* Interrupt req. control */
#define CFB_OFFSET_ROM		0x380000	/* Diagnostic ROM */
#define CFB_OFFSET_RESET	0x3c0000	/* Bt459 resets on writes */

/*
 * Generic register access
 */
void
bt459_select_reg(regs, regno)
	bt459_regmap_t *regs;
{
	regs->addr_lo = regno;
	regs->addr_hi = regno >> 8;
	MachEmptyWriteBuffer();
}

void
bt459_write_reg(regs, regno, val)
	bt459_regmap_t *regs;
{
	regs->addr_lo = regno;
	regs->addr_hi = regno >> 8;
	MachEmptyWriteBuffer();
	regs->addr_reg = val;
	MachEmptyWriteBuffer();
}

unsigned char
bt459_read_reg(regs, regno)
	bt459_regmap_t *regs;
{
	regs->addr_lo = regno;
	regs->addr_hi = regno >> 8;
	MachEmptyWriteBuffer();
	return regs->addr_reg;
}

#ifdef DEBUG
bt459_print_colormap(regs)
	bt459_regmap_t *regs;
{
	register int i;

	bt459_select_reg(regs, 0);
	for (i = 0; i < 256; i++) {
		register unsigned red, green, blue;

		red = regs->addr_cmap;
		green = regs->addr_cmap;
		blue = regs->addr_cmap;
		printf("%x->[x%x x%x x%x]\n", i, red, green, blue);
	}
}
#endif

/*
 * Test to see if device is present.
 * Return true if found and initialized ok.
 */
cfb_init(cp)
	register struct pmax_ctlr *cp;
{
	bt459_regmap_t *regs;

	/* check for no frame buffer */
	if (badaddr(cp->pmax_addr, 4))
		return (0);

	fb_addr = (unsigned)cp->pmax_addr + CFB_OFFSET_VRAM;
	planemask_addr = 0; /* XXX */
	regs = (bt459_regmap_t *)(fb_addr + CFB_OFFSET_BT459);

	if (bt459_read_reg(regs, BT459_REG_ID) != 0x4a)
		return (0);

	*(int *)(fb_addr + CFB_OFFSET_RESET) = 0;	/* force chip reset */
	DELAY(2000);	/* ???? check right time on specs! ???? */

	/* use 4:1 input mux */
	bt459_write_reg(regs, BT459_REG_CMD0, 0x40);

	/* no zooming, no panning */
	bt459_write_reg(regs, BT459_REG_CMD1, 0x00);

	/*
	 * signature test, X-windows cursor, no overlays, SYNC* PLL,
	 * normal RAM select, 7.5 IRE pedestal, do sync
	 */
	bt459_write_reg(regs, BT459_REG_CMD2, 0xc2);

	/* get all pixel bits */
	bt459_write_reg(regs, BT459_REG_PRM, 0xff);

	/* no blinking */
	bt459_write_reg(regs, BT459_REG_PBM, 0x00);

	/* no overlay */
	bt459_write_reg(regs, BT459_REG_ORM, 0x00);

	/* no overlay blink */
	bt459_write_reg(regs, BT459_REG_OBM, 0x00);

	/* no interleave, no underlay */
	bt459_write_reg(regs, BT459_REG_ILV, 0x00);

	/* normal operation, no signature analysis */
	bt459_write_reg(regs, BT459_REG_TEST, 0x00);

	/*
	 * no blinking, 1bit cross hair, XOR reg&crosshair,
	 * no crosshair on either plane 0 or 1,
	 * regular cursor on both planes.
	 */
	bt459_write_reg(regs, BT459_REG_CCR, 0xc0);

	/* home cursor */
	bt459_write_reg(regs, BT459_REG_CXLO, 0x00);
	bt459_write_reg(regs, BT459_REG_CXHI, 0x00);
	bt459_write_reg(regs, BT459_REG_CYLO, 0x00);
	bt459_write_reg(regs, BT459_REG_CYHI, 0x00);

	/* no crosshair window */
	bt459_write_reg(regs, BT459_REG_WXLO, 0x00);
	bt459_write_reg(regs, BT459_REG_WXHI, 0x00);
	bt459_write_reg(regs, BT459_REG_WYLO, 0x00);
	bt459_write_reg(regs, BT459_REG_WYHI, 0x00);
	bt459_write_reg(regs, BT459_REG_WWLO, 0x00);
	bt459_write_reg(regs, BT459_REG_WWHI, 0x00);
	bt459_write_reg(regs, BT459_REG_WHLO, 0x00);
	bt459_write_reg(regs, BT459_REG_WHHI, 0x00);

	/*
	 * Initialize screen info.
	 */
	pmu.scrInfo.max_row = MAX_ROW;
	pmu.scrInfo.max_col = MAX_COL;
	pmu.scrInfo.max_x = 1024;
	pmu.scrInfo.max_y = 864;
	pmu.scrInfo.max_cur_x = 1023;
	pmu.scrInfo.max_cur_y = 863;
	pmu.scrInfo.version = 11;
	pmu.scrInfo.mthreshold = 4;
	pmu.scrInfo.mscale = 2;
	pmu.scrInfo.min_cur_x = 0;
	pmu.scrInfo.min_cur_y = 0;
	pmu.scrInfo.qe.timestamp_ms = TO_MS(time);
	pmu.scrInfo.qe.eSize = PM_MAXEVQ;
	pmu.scrInfo.qe.eHead = pmu.scrInfo.qe.eTail = 0;
	pmu.scrInfo.qe.tcSize = MOTION_BUFFER_SIZE;
	pmu.scrInfo.qe.tcNext = 0;

	/*
	 * Initialize the color map, the screen, and the mouse.
	 */
	InitColorMap();
	ScreenInit();
	Scroll();

	initialized = 1;
	return (1);
}

/*
 * ----------------------------------------------------------------------------
 *
 * ScreenInit --
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
ScreenInit()
{

	/*
	 * Home the cursor.
	 * We want an LSI terminal emulation. We want the graphics
	 * terminal to scroll from the bottom. So start at the bottom.
	 */
	row = 55;
	col = 0;

	/*
	 * Load the cursor with the default values
	 *
	 */
	LoadCursor(defCursor);
}

/*
 * ----------------------------------------------------------------------------
 *
 * LoadCursor --
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
LoadCursor(cur)
	unsigned short *cur;
{
	bt459_regmap_t *regs = (bt459_regmap_t *)(fb_addr + CFB_OFFSET_BT459);
	register int i, j;

	/*
	 * As per specs, must run a check to see if we
	 * had contention. If so, re-write the cursor.
	 */
	for (j = 0; j < 2; j++) {
		/* loop once to write */
		bt459_select_reg(regs, BT459_REG_CRAM_BASE);
		for (i = 0; i < 1024; i++) {
			regs->addr_reg = cur[i];
			MachEmptyWriteBuffer();
		}

		/* loop to check, if fail write again */
		bt459_select_reg(regs, BT459_REG_CRAM_BASE);
		for (i = 0; i < 1024; i++)
			if (regs->addr_reg != cur[i])
				break;
		if (i == 1024)
			break;	/* all went well first shot */
	}
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
RestoreCursorColor()
{
	bt459_regmap_t *regs = (bt459_regmap_t *)(fb_addr + CFB_OFFSET_BT459);
	register int i;

	bt459_select_reg(regs, BT459_REG_CCOLOR_2);
	for (i = 0; i < 6; i++) {
		regs->addr_reg = cursor_RGB[i];
		MachEmptyWriteBuffer();
	}
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
CursorColor(color)
	unsigned int color[];
{
	register int i, j;

	for (i = 0; i < 6; i++)
		cursor_RGB[i] = (u_char)(color[i] >> 8);

	RestoreCursorColor();
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
static void
PosCursor(x, y)
	register int x, y;
{
	bt459_regmap_t *regs = (bt459_regmap_t *)(fb_addr + CFB_OFFSET_BT459);

	if (y < pmu.scrInfo.min_cur_y || y > pmu.scrInfo.max_cur_y)
		y = pmu.scrInfo.max_cur_y;
	if (x < pmu.scrInfo.min_cur_x || x > pmu.scrInfo.max_cur_x)
		x = pmu.scrInfo.max_cur_x;
	pmu.scrInfo.cursor.x = x;		/* keep track of real cursor */
	pmu.scrInfo.cursor.y = y;		/* position, indep. of mouse */

	x += 219;
	y += 34;

	bt459_select_reg(regs, BT459_REG_CXLO);
	regs->addr_reg = x;
	MachEmptyWriteBuffer();
	regs->addr_reg = x >> 8;
	MachEmptyWriteBuffer();
	regs->addr_reg = y;
	MachEmptyWriteBuffer();
	regs->addr_reg = y >> 8;
	MachEmptyWriteBuffer();
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
InitColorMap()
{
	bt459_regmap_t *regs = (bt459_regmap_t *)(fb_addr + CFB_OFFSET_BT459);
	register int i;

	bt459_select_reg(regs, 0);
	regs->addr_cmap = 0; MachEmptyWriteBuffer();
	regs->addr_cmap = 0; MachEmptyWriteBuffer();
	regs->addr_cmap = 0; MachEmptyWriteBuffer();

	for (i = 1; i < 256; i++) {
		regs->addr_cmap = 0xff; MachEmptyWriteBuffer();
		regs->addr_cmap = 0xff; MachEmptyWriteBuffer();
		regs->addr_cmap = 0xff; MachEmptyWriteBuffer();
	}

	for (i = 0; i < 3; i++) {
		cursor_RGB[i] = 0x00;
		cursor_RGB[i + 3] = 0xff;
	}
	RestoreCursorColor();
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
LoadColorMap(ptr)
	ColorMap *ptr;
{
	bt459_regmap_t *regs = (bt459_regmap_t *)(fb_addr + CFB_OFFSET_BT459);

	if (ptr->index > 256)
		return;

	bt459_select_reg(regs, ptr->index);

	regs->addr_cmap = ptr->Entry.red; MachEmptyWriteBuffer();
	regs->addr_cmap = ptr->Entry.green; MachEmptyWriteBuffer();
	regs->addr_cmap = ptr->Entry.blue; MachEmptyWriteBuffer();
}

/*
 * Video on/off state.
 */
struct vstate {
	u_char	color0[3];	/* saved color map entry zero */
	u_char	off;		/* TRUE if display is off */
} vstate;

/*
 * ----------------------------------------------------------------------------
 *
 * EnableVideo --
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
EnableVideo()
{
	bt459_regmap_t *regs = (bt459_regmap_t *)(fb_addr + CFB_OFFSET_BT459);

	if (!vstate.off)
		return;

	/* restore old color map entry zero */
	bt459_select_reg(regs, 0);
	regs->addr_cmap = vstate.color0[0];
	MachEmptyWriteBuffer();
	regs->addr_cmap = vstate.color0[1];
	MachEmptyWriteBuffer();
	regs->addr_cmap = vstate.color0[2];
	MachEmptyWriteBuffer();

	/* enable normal display */
	bt459_write_reg(regs, BT459_REG_PRM, 0xff);
	bt459_write_reg(regs, BT459_REG_CCR, 0xc0);

	vstate.off = 0;
}

/*
 * ----------------------------------------------------------------------------
 *
 * DisableVideo --
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
DisableVideo()
{
	bt459_regmap_t *regs = (bt459_regmap_t *)(fb_addr + CFB_OFFSET_BT459);

	if (vstate.off)
		return;

	/* save old color map entry zero */
	bt459_select_reg(regs, 0);
	vstate.color0[0] = regs->addr_cmap;
	vstate.color0[1] = regs->addr_cmap;
	vstate.color0[2] = regs->addr_cmap;

	/* set color map entry zero to zero */
	bt459_select_reg(regs, 0);
	regs->addr_cmap = 0;
	MachEmptyWriteBuffer();
	regs->addr_cmap = 0;
	MachEmptyWriteBuffer();
	regs->addr_cmap = 0;
	MachEmptyWriteBuffer();

	/* disable display */
	bt459_write_reg(regs, BT459_REG_PRM, 0);
	bt459_write_reg(regs, BT459_REG_CCR, 0);

	vstate.off = 1;
}
#endif
