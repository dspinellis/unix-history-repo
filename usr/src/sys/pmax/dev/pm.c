/* 
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pm.c	7.7 (Berkeley) %G%
 *
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

#include "pm.h"
#if NPM > 0

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
#include <pmax/dev/pmreg.h>
#include <pmax/dev/font.c>

#define MAX_ROW	56
#define MAX_COL	80

/*
 * Macro to translate from a time struct to milliseconds.
 */
#define TO_MS(tv) ((tv.tv_sec * 1000) + (tv.tv_usec / 1000))

static u_short	curReg;		/* copy of PCCRegs.cmdr since it's read only */
static int	isMono;		/* true if B&W frame buffer */
static int	initialized;	/* true if 'probe' was successful */
static int	GraphicsOpen;	/* true if the graphics device is open */
static int	row, col;	/* row and col for console cursor */
static struct	selinfo pm_selp;/* process waiting for select */

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
static void VDACInit();
static void LoadColorMap();
static void EnableVideo();
static void DisableVideo();

extern void dcKBDPutc();
extern void (*dcDivertXInput)();
extern void (*dcMouseEvent)();
extern void (*dcMouseButtons)();

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

	if (!initialized && !pminit())
		return (0);
	if (isMono)
		printf("pm0 (monochrome display)\n");
	else
		printf("pm0 (color display)\n");
	return (1);
}

/*
 *----------------------------------------------------------------------
 *
 * pmKbdEvent --
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
pmKbdEvent(ch)
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
	selwakeup(&pm_selp);
}

/*
 *----------------------------------------------------------------------
 *
 * pmMouseEvent --
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
pmMouseEvent(newRepPtr) 
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
	selwakeup(&pm_selp);
}

/*
 *----------------------------------------------------------------------
 *
 * pmMouseButtons --
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
pmMouseButtons(newRepPtr)
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
	selwakeup(&pm_selp);

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
	src = (int *)(MACH_UNCACHED_FRAME_BUFFER_ADDR + line);
	dest = (int *)(MACH_UNCACHED_FRAME_BUFFER_ADDR);
	end = (int *)(MACH_UNCACHED_FRAME_BUFFER_ADDR + (60 * line) - line);
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
	bzero(MACH_UNCACHED_FRAME_BUFFER_ADDR + (row * line), 3 * line);
}

/*
 *----------------------------------------------------------------------
 *
 * pmPutc --
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
pmPutc(c)
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
		bRow = (char *)(MACH_UNCACHED_FRAME_BUFFER_ADDR +
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
pmopen(dev, flag)
	dev_t dev;
	int flag;
{
	int s;

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
	bzero((caddr_t)MACH_UNCACHED_FRAME_BUFFER_ADDR,
		(isMono ? 1024 / 8 : 1024) * 864);
	PosCursor(col * 8, row * 15);
	return (0);
}

/*ARGSUSED*/
pmioctl(dev, cmd, data, flag)
	dev_t dev;
	caddr_t data;
{
	register PCCRegs *pcc = (PCCRegs *)MACH_CURSOR_REG_ADDR;
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
		addr = vmUserMap(4, (unsigned)MACH_PLANE_MASK_ADDR);
		if (addr == (caddr_t)0)
			goto mapError;
		pmu.scrInfo.planemask = (char *)addr;
		/*
		 * Map the frame buffer into the user's address space.
		 */
		addr = vmUserMap(isMono ? 256*1024 : 1024*1024,
			(unsigned)MACH_UNCACHED_FRAME_BUFFER_ADDR);
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
		if (!isMono)
			RestoreCursorColor();
		curReg |= PCC_ENPA;
		curReg &= ~PCC_FOPB;
		pcc->cmdr = curReg;
		break;

	case QIOVIDEOOFF:
		if (!isMono)
			VDACInit();
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

pmselect(dev, flag, p)
	dev_t dev;
	int flag;
	struct proc *p;
{

	switch (flag) {
	case FREAD:
		if (pmu.scrInfo.qe.eHead != pmu.scrInfo.qe.eTail)
			return (1);
		selrecord(p, &pm_selp);
		break;
	}

	return (0);
}

static u_char	bg_RGB[3];	/* background color for the cursor */
static u_char	fg_RGB[3];	/* foreground color for the cursor */

/*
 * The default cursor.
 */
unsigned short defCursor[32] = { 
/* plane A */ 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF,
	      0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF,
/* plane B */ 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF,
              0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF

};

/*
 * Test to see if device is present.
 * Return true if found and initialized ok.
 */
pminit()
{
	register PCCRegs *pcc = (PCCRegs *)MACH_CURSOR_REG_ADDR;

	isMono = *(u_short *)MACH_SYS_CSR_ADDR & MACH_CSR_MONO;
	if (isMono) {
		/* check for no frame buffer */
		if (badaddr((char *)MACH_UNCACHED_FRAME_BUFFER_ADDR, 4))
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
	pmu.scrInfo.max_row = 56;
	pmu.scrInfo.max_col = 80;
	pmu.scrInfo.max_x = 1024;
	pmu.scrInfo.max_y = 864;
	pmu.scrInfo.max_cur_x = 1023;
	pmu.scrInfo.max_cur_y = 863;
	pmu.scrInfo.version = 11;
	pmu.scrInfo.mthreshold = 4;	
	pmu.scrInfo.mscale = 2;
	pmu.scrInfo.min_cur_x = -15;
	pmu.scrInfo.min_cur_y = -15;
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
	 * We want an LSI terminal emulation.  We want the graphics
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
	register PCCRegs *pcc = (PCCRegs *)MACH_CURSOR_REG_ADDR;
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
	register VDACRegs *vdac = (VDACRegs *)MACH_COLOR_MAP_ADDR;
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

	for (i = 0; i < 3; i++)
		bg_RGB[i] = (u_char)(color[i] >> 8);

	for (i = 3, j = 0; i < 6; i++, j++)
		fg_RGB[j] = (u_char)(color[i] >> 8);

	RestoreCursorColor();
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
 *	The colormap is initialized appropriately whether it is color or 
 *	monochrome.
 *
 * ----------------------------------------------------------------------------
 */
static void
InitColorMap()
{
	register VDACRegs *vdac = (VDACRegs *)MACH_COLOR_MAP_ADDR;
	register int i;

	*(char *)MACH_PLANE_MASK_ADDR = 0xff;
	MachEmptyWriteBuffer();

	if (isMono) {
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
	RestoreCursorColor();
}

/*
 * ----------------------------------------------------------------------------
 *
 * VDACInit --
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
VDACInit()
{
	register VDACRegs *vdac = (VDACRegs *)MACH_COLOR_MAP_ADDR;

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
	register VDACRegs *vdac = (VDACRegs *)MACH_COLOR_MAP_ADDR;

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
	register PCCRegs *pcc = (PCCRegs *)MACH_CURSOR_REG_ADDR;

	if (y < pmu.scrInfo.min_cur_y || y > pmu.scrInfo.max_cur_y)
		y = pmu.scrInfo.max_cur_y;
	if (x < pmu.scrInfo.min_cur_x || x > pmu.scrInfo.max_cur_x)
		x = pmu.scrInfo.max_cur_x;
	pmu.scrInfo.cursor.x = x;		/* keep track of real cursor */
	pmu.scrInfo.cursor.y = y;		/* position, indep. of mouse */
	pcc->xpos = PCC_X_OFFSET + x;
	pcc->ypos = PCC_Y_OFFSET + y;
}
#endif
