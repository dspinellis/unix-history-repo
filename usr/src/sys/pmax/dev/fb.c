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
 *	@(#)fb.c	8.1 (Berkeley) 6/10/93
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
 * This file has all the routines common to the various frame buffer drivers
 * including a generic ioctl routine. The pmax_fb structure is passed into the
 * routines and has device specifics stored in it.
 * The LK201 keycode mapping routine is also here along with initialization
 * functions for the keyboard and mouse.
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/ioctl.h>
#include <sys/tty.h>
#include <sys/time.h>
#include <sys/kernel.h>
#include <sys/ioctl.h>
#include <sys/file.h>
#include <sys/vnode.h>
#include <sys/errno.h>
#include <sys/proc.h>
#include <sys/mman.h>
#include <sys/syslog.h>

#include <vm/vm.h>
#include <miscfs/specfs/specdev.h>

#include <machine/machConst.h>
#include <machine/pmioctl.h>

#include <pmax/dev/device.h>
#include <pmax/dev/font.c>
#include <pmax/dev/fbreg.h>

#include <pmax/stand/dec_prom.h>

#include <pmax/pmax/cons.h>
#include <pmax/pmax/pmaxtype.h>

#include <dc.h>
#include <scc.h>
#include <dtop.h>

void fbKbdEvent(), fbMouseEvent(), fbMouseButtons(), fbScroll();
void fbBlitc(), fbPutc();
extern int pmax_boardtype;
extern struct consdev cn_tab;
#if NDC > 0
#include <machine/dc7085cons.h>
extern int dcGetc(), dcparam();
extern void dcPutc();
#endif
#if NDTOP > 0
#include <pmax/dev/dtopreg.h>
extern void dtopKBDPutc();
#endif
#if NSCC > 0
#include <pmax/dev/sccreg.h>
extern int sccGetc(), sccparam();
extern void sccPutc();
#endif

/*
 * The default cursor.
 */
u_short defCursor[32] = { 
/* plane A */ 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF,
	      0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF,
/* plane B */ 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF,
              0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF

};

/*
 * Font mask bits used by fbBlitc().
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
 * Ascii values of command keys.
 */
#define KBD_TAB		'\t'
#define KBD_DEL		127
#define KBD_RET		'\r'

/*
 *  Define "hardware-independent" codes for the control, shift, meta and
 *  function keys.  Codes start after the last 7-bit ASCII code (127)
 *  and are assigned in an arbitrary order.
 */
#define KBD_NOKEY	128

#define KBD_F1		201
#define KBD_F2		202
#define KBD_F3		203
#define KBD_F4		204
#define KBD_F5		205
#define KBD_F6		206
#define KBD_F7		207
#define KBD_F8		208
#define KBD_F9		209
#define KBD_F10		210
#define KBD_F11		211
#define KBD_F12		212
#define KBD_F13		213
#define KBD_F14		214
#define KBD_HELP	215
#define KBD_DO		216
#define KBD_F17		217
#define KBD_F18		218
#define KBD_F19		219
#define KBD_F20		220

#define KBD_FIND	221
#define KBD_INSERT	222
#define KBD_REMOVE	223
#define KBD_SELECT	224
#define KBD_PREVIOUS	225
#define KBD_NEXT	226

#define KBD_KP_ENTER	227
#define KBD_KP_F1	228
#define KBD_KP_F2	229
#define KBD_KP_F3	230
#define KBD_KP_F4	231
#define KBD_LEFT	232
#define KBD_RIGHT	233
#define KBD_DOWN	234
#define KBD_UP		235

#define KBD_CONTROL	236
#define KBD_SHIFT	237
#define KBD_CAPSLOCK	238
#define KBD_ALTERNATE	239

/*
 * Keyboard to Ascii, unshifted.
 */
static unsigned char unshiftedAscii[] = {
/*  0 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/*  4 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/*  8 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/*  c */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 10 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 14 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 18 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 1c */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 20 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 24 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 28 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 2c */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 30 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 34 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 38 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 3c */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 40 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 44 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 48 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 4c */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 50 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 54 */ KBD_NOKEY,	KBD_NOKEY,	KBD_F1,		KBD_F2,
/* 58 */ KBD_F3,	KBD_F4,		KBD_F5,		KBD_NOKEY,
/* 5c */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 60 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 64 */ KBD_F6,	KBD_F7,		KBD_F8,		KBD_F9,
/* 68 */ KBD_F10,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 6c */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 70 */ KBD_NOKEY,	'\033',		KBD_F12,	KBD_F13,
/* 74 */ KBD_F14,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 78 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 7c */ KBD_HELP,	KBD_DO,		KBD_NOKEY,	KBD_NOKEY,
/* 80 */ KBD_F17,	KBD_F18,	KBD_F19,	KBD_F20,
/* 84 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 88 */ KBD_NOKEY,	KBD_NOKEY,	KBD_FIND,	KBD_INSERT,
/* 8c */ KBD_REMOVE,	KBD_SELECT,	KBD_PREVIOUS,	KBD_NEXT,
/* 90 */ KBD_NOKEY,	KBD_NOKEY,	'0',		KBD_NOKEY,
/* 94 */ '.',		KBD_KP_ENTER,	'1',		'2',
/* 98 */ '3',		'4',		'5',		'6',
/* 9c */ ',',		'7',		'8',		'9',
/* a0 */ '-',		KBD_KP_F1,	KBD_KP_F2,	KBD_KP_F3,
/* a4 */ KBD_KP_F4,	KBD_NOKEY,	KBD_NOKEY,	KBD_LEFT,
/* a8 */ KBD_RIGHT,	KBD_DOWN, 	KBD_UP,		KBD_NOKEY,
/* ac */ KBD_NOKEY,	KBD_NOKEY,	KBD_SHIFT,	KBD_CONTROL,
/* b0 */ KBD_CAPSLOCK,	KBD_ALTERNATE,	KBD_NOKEY,	KBD_NOKEY,
/* b4 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* b8 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* bc */ KBD_DEL,	KBD_RET,	KBD_TAB,	'`',
/* c0 */ '1',		'q',		'a',		'z',
/* c4 */ KBD_NOKEY,	'2',		'w',		's',
/* c8 */ 'x',		'<',		KBD_NOKEY,	'3',
/* cc */ 'e',		'd',		'c',		KBD_NOKEY,
/* d0 */ '4',		'r',		'f',		'v',
/* d4 */ ' ',		KBD_NOKEY,	'5',		't',
/* d8 */ 'g',		'b',		KBD_NOKEY,	'6',
/* dc */ 'y',		'h',		'n',		KBD_NOKEY,
/* e0 */ '7',		'u',		'j',		'm',
/* e4 */ KBD_NOKEY,	'8',		'i',		'k',
/* e8 */ ',',		KBD_NOKEY,	'9',		'o',
/* ec */ 'l',		'.',		KBD_NOKEY,	'0',
/* f0 */ 'p',		KBD_NOKEY,	';',		'/',
/* f4 */ KBD_NOKEY,	'=',		']',		'\\',
/* f8 */ KBD_NOKEY,	'-',		'[',		'\'',
/* fc */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
};

/*
 * Keyboard to Ascii, shifted.
 */
static unsigned char shiftedAscii[] = {
/*  0 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/*  4 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/*  8 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/*  c */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 10 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 14 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 18 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 1c */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 20 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 24 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 28 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 2c */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 30 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 34 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 38 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 3c */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 40 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 44 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 48 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 4c */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 50 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 54 */ KBD_NOKEY,	KBD_NOKEY,	KBD_F1,		KBD_F2,
/* 58 */ KBD_F3,	KBD_F4,		KBD_F5,		KBD_NOKEY,
/* 5c */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 60 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 64 */ KBD_F6,	KBD_F7,		KBD_F8,		KBD_F9,
/* 68 */ KBD_F10,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 6c */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 70 */ KBD_NOKEY,	KBD_F11,	KBD_F12,	KBD_F13,
/* 74 */ KBD_F14,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 78 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 7c */ KBD_HELP,	KBD_DO,		KBD_NOKEY,	KBD_NOKEY,
/* 80 */ KBD_F17,	KBD_F18,	KBD_F19,	KBD_F20,
/* 84 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* 88 */ KBD_NOKEY,	KBD_NOKEY,	KBD_FIND,	KBD_INSERT,
/* 8c */ KBD_REMOVE,	KBD_SELECT,	KBD_PREVIOUS,	KBD_NEXT,
/* 90 */ KBD_NOKEY,	KBD_NOKEY,	'0',		KBD_NOKEY,
/* 94 */ '.',		KBD_KP_ENTER,	'1',		'2',
/* 98 */ '3',		'4',		'5',		'6',
/* 9c */ ',',		'7',		'8',		'9',
/* a0 */ '-',		KBD_KP_F1,	KBD_KP_F2,	KBD_KP_F3,
/* a4 */ KBD_KP_F4,	KBD_NOKEY,	KBD_NOKEY,	KBD_LEFT,
/* a8 */ KBD_RIGHT,	KBD_DOWN, 	KBD_UP,		KBD_NOKEY,
/* ac */ KBD_NOKEY,	KBD_NOKEY,	KBD_SHIFT,	KBD_CONTROL,
/* b0 */ KBD_CAPSLOCK,	KBD_ALTERNATE,	KBD_NOKEY,	KBD_NOKEY,
/* b4 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* b8 */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
/* bc */ KBD_DEL,	KBD_RET,	KBD_TAB,	'~',
/* c0 */ '!',		'q',		'a',		'z',
/* c4 */ KBD_NOKEY,	'@',		'w',		's',
/* c8 */ 'x',		'>',		KBD_NOKEY,	'#',
/* cc */ 'e',		'd',		'c',		KBD_NOKEY,
/* d0 */ '$',		'r',		'f',		'v',
/* d4 */ ' ',		KBD_NOKEY,	'%',		't',
/* d8 */ 'g',		'b',		KBD_NOKEY,	'^',
/* dc */ 'y',		'h',		'n',		KBD_NOKEY,
/* e0 */ '&',		'u',		'j',		'm',
/* e4 */ KBD_NOKEY,	'*',		'i',		'k',
/* e8 */ '<',		KBD_NOKEY,	'(',		'o',
/* ec */ 'l',		'>',		KBD_NOKEY,	')',
/* f0 */ 'p',		KBD_NOKEY,	':',		'?',
/* f4 */ KBD_NOKEY,	'+',		'}',		'|',
/* f8 */ KBD_NOKEY,	'_',		'{',		'"',
/* fc */ KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,	KBD_NOKEY,
};

/* 
 * Keyboard initialization string.
 */
static u_char kbdInitString[] = {
	LK_LED_ENABLE, LED_ALL,		/* show we are resetting keyboard */
	LK_DEFAULTS,
	LK_CMD_MODE(LK_AUTODOWN, 1), 
	LK_CMD_MODE(LK_AUTODOWN, 2), 
	LK_CMD_MODE(LK_AUTODOWN, 3), 
	LK_CMD_MODE(LK_DOWN, 4),	/* could also be LK_AUTODOWN */
	LK_CMD_MODE(LK_UPDOWN, 5),   
	LK_CMD_MODE(LK_UPDOWN, 6),   
	LK_CMD_MODE(LK_AUTODOWN, 7), 
	LK_CMD_MODE(LK_AUTODOWN, 8), 
	LK_CMD_MODE(LK_AUTODOWN, 9), 
	LK_CMD_MODE(LK_AUTODOWN, 10), 
	LK_CMD_MODE(LK_AUTODOWN, 11), 
	LK_CMD_MODE(LK_AUTODOWN, 12), 
	LK_CMD_MODE(LK_DOWN, 13), 
	LK_CMD_MODE(LK_AUTODOWN, 14),
	LK_AR_ENABLE,			/* we want autorepeat by default */
	LK_CL_ENABLE, 0x83,		/* keyclick, volume */
	LK_KBD_ENABLE,			/* the keyboard itself */
	LK_BELL_ENABLE, 0x83,		/* keyboard bell, volume */
	LK_LED_DISABLE, LED_ALL,	/* clear keyboard leds */
};

/*
 *----------------------------------------------------------------------
 *
 * fbKbdEvent --
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
fbKbdEvent(ch, fp)
	int ch;
	register struct pmax_fb *fp;
{
	register pmEvent *eventPtr;
	int i;

	if (!fp->GraphicsOpen)
		return;

	/*
	 * See if there is room in the queue.
	 */
	i = PM_EVROUND(fp->fbu->scrInfo.qe.eTail + 1);
	if (i == fp->fbu->scrInfo.qe.eHead)
		return;

	/*
	 * Add the event to the queue.
	 */
	eventPtr = &fp->fbu->events[fp->fbu->scrInfo.qe.eTail];
	eventPtr->type = BUTTON_RAW_TYPE;
	eventPtr->device = KEYBOARD_DEVICE;
	eventPtr->x = fp->fbu->scrInfo.mouse.x;
	eventPtr->y = fp->fbu->scrInfo.mouse.y;
	eventPtr->time = TO_MS(time);
	eventPtr->key = ch;
	fp->fbu->scrInfo.qe.eTail = i;
	selwakeup(&fp->selp);
}

/*
 *----------------------------------------------------------------------
 *
 * fbMouseEvent --
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
fbMouseEvent(newRepPtr, fp) 
	register MouseReport *newRepPtr;
	register struct pmax_fb *fp;
{
	unsigned milliSec;
	int i;
	pmEvent *eventPtr;

	if (!fp->GraphicsOpen)
		return;

	milliSec = TO_MS(time);

	/*
	 * Check to see if we have to accelerate the mouse
	 */
	if (fp->fbu->scrInfo.mscale >= 0) {
		if (newRepPtr->dx >= fp->fbu->scrInfo.mthreshold) {
			newRepPtr->dx +=
				(newRepPtr->dx - fp->fbu->scrInfo.mthreshold) *
				fp->fbu->scrInfo.mscale;
		}
		if (newRepPtr->dy >= fp->fbu->scrInfo.mthreshold) {
			newRepPtr->dy +=
				(newRepPtr->dy - fp->fbu->scrInfo.mthreshold) *
				fp->fbu->scrInfo.mscale;
		}
	}

	/*
	 * Update mouse position
	 */
	if (newRepPtr->state & MOUSE_X_SIGN) {
		fp->fbu->scrInfo.mouse.x += newRepPtr->dx;
		if (fp->fbu->scrInfo.mouse.x > fp->fbu->scrInfo.max_cur_x)
			fp->fbu->scrInfo.mouse.x = fp->fbu->scrInfo.max_cur_x;
	} else {
		fp->fbu->scrInfo.mouse.x -= newRepPtr->dx;
		if (fp->fbu->scrInfo.mouse.x < fp->fbu->scrInfo.min_cur_x)
			fp->fbu->scrInfo.mouse.x = fp->fbu->scrInfo.min_cur_x;
	}
	if (newRepPtr->state & MOUSE_Y_SIGN) {
		fp->fbu->scrInfo.mouse.y -= newRepPtr->dy;
		if (fp->fbu->scrInfo.mouse.y < fp->fbu->scrInfo.min_cur_y)
			fp->fbu->scrInfo.mouse.y = fp->fbu->scrInfo.min_cur_y;
	} else {
		fp->fbu->scrInfo.mouse.y += newRepPtr->dy;
		if (fp->fbu->scrInfo.mouse.y > fp->fbu->scrInfo.max_cur_y)
			fp->fbu->scrInfo.mouse.y = fp->fbu->scrInfo.max_cur_y;
	}

	/*
	 * Move the hardware cursor.
	 */
	(*fp->posCursor)(fp->fbu->scrInfo.mouse.x, fp->fbu->scrInfo.mouse.y);

	/*
	 * Store the motion event in the motion buffer.
	 */
	fp->fbu->tcs[fp->fbu->scrInfo.qe.tcNext].time = milliSec;
	fp->fbu->tcs[fp->fbu->scrInfo.qe.tcNext].x = fp->fbu->scrInfo.mouse.x;
	fp->fbu->tcs[fp->fbu->scrInfo.qe.tcNext].y = fp->fbu->scrInfo.mouse.y;
	if (++fp->fbu->scrInfo.qe.tcNext >= MOTION_BUFFER_SIZE)
		fp->fbu->scrInfo.qe.tcNext = 0;
	if (fp->fbu->scrInfo.mouse.y < fp->fbu->scrInfo.mbox.bottom &&
	    fp->fbu->scrInfo.mouse.y >=  fp->fbu->scrInfo.mbox.top &&
	    fp->fbu->scrInfo.mouse.x < fp->fbu->scrInfo.mbox.right &&
	    fp->fbu->scrInfo.mouse.x >=  fp->fbu->scrInfo.mbox.left)
		return;

	fp->fbu->scrInfo.mbox.bottom = 0;
	if (PM_EVROUND(fp->fbu->scrInfo.qe.eTail + 1) == fp->fbu->scrInfo.qe.eHead)
		return;

	i = PM_EVROUND(fp->fbu->scrInfo.qe.eTail - 1);
	if ((fp->fbu->scrInfo.qe.eTail != fp->fbu->scrInfo.qe.eHead) && 
	    (i != fp->fbu->scrInfo.qe.eHead)) {
		pmEvent *eventPtr;

		eventPtr = &fp->fbu->events[i];
		if (eventPtr->type == MOTION_TYPE) {
			eventPtr->x = fp->fbu->scrInfo.mouse.x;
			eventPtr->y = fp->fbu->scrInfo.mouse.y;
			eventPtr->time = milliSec;
			eventPtr->device = MOUSE_DEVICE;
			return;
		}
	}
	/*
	 * Put event into queue and wakeup any waiters.
	 */
	eventPtr = &fp->fbu->events[fp->fbu->scrInfo.qe.eTail];
	eventPtr->type = MOTION_TYPE;
	eventPtr->time = milliSec;
	eventPtr->x = fp->fbu->scrInfo.mouse.x;
	eventPtr->y = fp->fbu->scrInfo.mouse.y;
	eventPtr->device = MOUSE_DEVICE;
	fp->fbu->scrInfo.qe.eTail = PM_EVROUND(fp->fbu->scrInfo.qe.eTail + 1);
	selwakeup(&fp->selp);
}

/*
 *----------------------------------------------------------------------
 *
 * fbMouseButtons --
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
fbMouseButtons(newRepPtr, fp)
	MouseReport *newRepPtr;
	register struct pmax_fb *fp;
{
	static char temp, oldSwitch, newSwitch;
	int i, j;
	pmEvent *eventPtr;
	static MouseReport lastRep;

	if (!fp->GraphicsOpen)
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
		i = PM_EVROUND(fp->fbu->scrInfo.qe.eTail+1);
		if (i == fp->fbu->scrInfo.qe.eHead)
			return;

		/*
		 * Put event into queue.
		 */
		eventPtr = &fp->fbu->events[fp->fbu->scrInfo.qe.eTail];

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
		eventPtr->x = fp->fbu->scrInfo.mouse.x;
		eventPtr->y = fp->fbu->scrInfo.mouse.y;
		fp->fbu->scrInfo.qe.eTail = i;
	}
	selwakeup(&fp->selp);

	lastRep = *newRepPtr;
	fp->fbu->scrInfo.mswitches = newSwitch;
}

/*
 *----------------------------------------------------------------------
 *
 * fbScroll --
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
void
fbScroll(fp)
	register struct pmax_fb *fp;
{
	register int *dest, *src;
	register int *end;
	register int temp0, temp1, temp2, temp3;
	register int i, scanInc, lineCount;
	int line;

	/*
	 * If the mouse is on we don't scroll so that the bit map remains sane.
	 */
	if (fp->GraphicsOpen) {
		fp->row = 0;
		return;
	}

	/*
	 *  The following is an optimization to cause the scrolling 
	 *  of text to be memory limited.  Basically the writebuffer is 
	 *  4 words (32 bits ea.) long so to achieve maximum speed we 
	 *  read and write in multiples of 4 words. We also limit the 
	 *  size to be fp->fbu->scrInfo.max_col characters for more speed. 
	 */
	if (fp->isMono) {
		lineCount = 5;
		line = 1920 * 2;
		scanInc = 44;
	} else {
		lineCount = 40;
		if (fp->fbu->scrInfo.max_x > 1024) {
			scanInc = 352;
			line = 1920 * 16;
		} else {
			scanInc = 96;
			line = 1920 * 8;
		}
	}
	src = (int *)(fp->fr_addr + line);
	dest = (int *)(fp->fr_addr);
	end = (int *)(fp->fr_addr + (68 * line) - line);
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
	bzero(fp->fr_addr + (fp->row * line), 3 * line);
}

/*
 *----------------------------------------------------------------------
 *
 * fbPutc --
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
void
fbPutc(dev, c)
	dev_t dev;
	register int c;
{
	int s;

	if (cn_tab.cn_fb) {
		static int recurse;

		/*
		 * We need to prevent recursion in case a printf to the
		 * console happens at interrupt time but using splhigh()
		 * all the time locks out interrupts too much. We simply
		 * discard the character in this case and rely on the
		 * console log buffer to save the message.
		 */
		if (recurse)
			return;
		recurse = 1;
		fbBlitc(c, cn_tab.cn_fb);
		recurse = 0;
	} else {
		s = splhigh();
		(*callv->printf)("%c", c);
		splx(s);
	}
}

/*
 *----------------------------------------------------------------------
 *
 * fbBlitc --
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
void
fbBlitc(c, fp)
	register int c;
	register struct pmax_fb *fp;
{
	register char *bRow, *fRow;
	register int i;
	register int ote;
	int colMult = fp->isMono ? 1 : 8;

	if (fp->isMono)
		ote = 256;
	else
		ote = ((fp->fbu->scrInfo.max_x + 1023) / 1024) * 1024;
	c &= 0xff;

	switch (c) {
	case '\t':
		for (i = 8 - (fp->col & 0x7); i > 0; i--)
			fbBlitc(' ', fp);
		break;

	case '\r':
		fp->col = 0;
		break;

	case '\b':
		fp->col--;
		if (fp->col < 0)
			fp->col = 0;
		break;

	case '\n':
		if (fp->row + 1 >= fp->fbu->scrInfo.max_row)
			fbScroll(fp);
		else
			fp->row++;
		fp->col = 0;
		break;

	case '\007':
		(*fp->KBDPutc)(fp->kbddev, LK_RING_BELL);
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
		 * increment fp->row counter or scroll screen.
		 */
		if (fp->col >= fp->fbu->scrInfo.max_col) {
			fp->col = 0;
			if (fp->row + 1 >= fp->fbu->scrInfo.max_row)
				fbScroll(fp);
			else
				fp->row++;
		}
		bRow = (char *)(fp->fr_addr +
			(fp->row * 15 & 0x3ff) * ote + fp->col * colMult);
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
		if (fp->isMono) {
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
				 * We add 256/512 to the pointer to
				 * point to the pixel on the 
				 * next scan line
				 * directly below the current
				 * pixel.
				 */
				pInt[0] = fontmaskBits[(*fRow) & 0xf];
				pInt[1] = fontmaskBits[((*fRow) >> 4) & 0xf];
				fRow++; 
				if (fp->fbu->scrInfo.max_x > 1024)
					pInt += 512;
				else
					pInt += 256;
			}
		}
		fp->col++; /* increment column counter */
	}
	if (!fp->GraphicsOpen)
		(*fp->posCursor)(fp->col * 8, fp->row * 15);
}

/*
 * ----------------------------------------------------------------------------
 *
 * kbdMapChar --
 *
 *	Map characters from the keyboard to ASCII. Return -1 if there is
 *	no valid mapping.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Remember state of shift and control keys.
 *
 * ----------------------------------------------------------------------------
 */
kbdMapChar(cc)
	int cc;
{
	static u_char shiftDown;
	static u_char ctrlDown;
	static u_char lastChar;

	switch (cc) {
	case KEY_REPEAT:
		cc = lastChar;
		goto done;

	case KEY_UP:
		shiftDown = 0;
		ctrlDown = 0;
		return (-1);

	case KEY_SHIFT:
	case KEY_R_SHIFT:
		if (ctrlDown || shiftDown)
			shiftDown = 0;
		else
			shiftDown = 1;
		return (-1);

	case KEY_CONTROL:
		if (shiftDown || ctrlDown)
			ctrlDown = 0;
		else
			ctrlDown = 1;
		return (-1);

	case LK_POWER_ERROR:
	case LK_KDOWN_ERROR:
	case LK_INPUT_ERROR:
	case LK_OUTPUT_ERROR:
		log(LOG_WARNING,
			"lk201: keyboard error, code=%x\n", cc);
		return (-1);
	}
	if (shiftDown)
		cc = shiftedAscii[cc];
	else
		cc = unshiftedAscii[cc];
	if (cc >= KBD_NOKEY) {
		/*
		 * A function key was typed - ignore it.
		 */
		return (-1);
	}
	if (cc >= 'a' && cc <= 'z') {
		if (ctrlDown)
			cc = cc - 'a' + '\1'; /* ^A */
		else if (shiftDown)
			cc = cc - 'a' + 'A';
	} else if (ctrlDown) {
		if (cc >= '[' && cc <= '_')
			cc = cc - '@';
		else if (cc == ' ' || cc == '@')
			cc = '\0';
	}
	lastChar = cc;
done:
	return (cc);
}

/*
 * Initialize the Keyboard.
 */
void
KBDReset(kbddev, putc)
	dev_t kbddev;
	void (*putc)();
{
	register int i;
	static int inKBDReset;

	if (inKBDReset)
		return;
	inKBDReset = 1;
	for (i = 0; i < sizeof(kbdInitString); i++)
		(*putc)(kbddev, (int)kbdInitString[i]);
	inKBDReset = 0;
}

/*
 * Initialize the mouse.
 */
void
MouseInit(mdev, putc, getc)
	dev_t mdev;
	void (*putc)();
	int (*getc)();
{
	int id_byte1, id_byte2, id_byte3, id_byte4;

	/*
	 * Initialize the mouse.
	 */
	(*putc)(mdev, MOUSE_SELF_TEST);
	id_byte1 = (*getc)(mdev);
	if (id_byte1 < 0) {
		printf("MouseInit: Timeout on 1st byte of self-test report\n");
		return;
	}
	id_byte2 = (*getc)(mdev);
	if (id_byte2 < 0) {
		printf("MouseInit: Timeout on 2nd byte of self-test report\n");
		return;
	}
	id_byte3 = (*getc)(mdev);
	if (id_byte3 < 0) {
		printf("MouseInit: Timeout on 3rd byte of self-test report\n");
		return;
	}
	id_byte4 = (*getc)(mdev);
	if (id_byte4 < 0) {
		printf("MouseInit: Timeout on 4th byte of self-test report\n");
		return;
	}
	if ((id_byte2 & 0x0f) != 0x2)
		printf("MouseInit: We don't have a mouse!!!\n");
	/*
	 * For some reason, the mouse doesn't see this command if it comes
	 * too soon after a self test.
	 */
	DELAY(100);
	(*putc)(mdev, MOUSE_INCREMENTAL);
}

/*
 * Get a character off of the keyboard.
 */
int
KBDGetc()
{
	register int c;

	for (;;) {
		c = (*cn_tab.cn_kbdgetc)(cn_tab.cn_dev);
		if (c == 0)
			return (-1);
		if ((c = kbdMapChar(c & 0xff)) >= 0)
			break;
	}
	return (c);
}

/*
 * Configure the keyboard/mouse based on machine type for turbochannel
 * display boards.
 */
tb_kbdmouseconfig(fp)
	struct pmax_fb *fp;
{

	switch (pmax_boardtype) {
#if NDC > 0
	case DS_3MAX:
		fp->KBDPutc = dcPutc;
		fp->kbddev = makedev(DCDEV, DCKBD_PORT);
		break;
#endif
#if NSCC > 0
	case DS_3MIN:
	case DS_3MAXPLUS:
		fp->KBDPutc = sccPutc;
		fp->kbddev = makedev(SCCDEV, SCCKBD_PORT);
		break;
#endif
#if NDTOP > 0
	case DS_MAXINE:
		fp->KBDPutc = dtopKBDPutc;
		fp->kbddev = makedev(DTOPDEV, DTOPKBD_PORT);
		break;
#endif
	default:
		printf("Can't configure keyboard/mouse\n");
		return (1);
	};
	return (0);
}

/*
 * Use vm_mmap() to map the frame buffer and shared data into the user's
 * address space.
 * Return errno if there was an error.
 */
fbmmap(fp, dev, data, p)
	struct pmax_fb *fp;
	dev_t dev;
	caddr_t data;
	struct proc *p;
{
	int error;
	vm_offset_t addr;
	vm_size_t len;
	struct vnode vn;
	struct specinfo si;
	struct fbuaccess *fbp;

	len = pmax_round_page(((vm_offset_t)fp->fbu & PGOFSET) +
		sizeof(struct fbuaccess)) + pmax_round_page(fp->fr_size);
	addr = (vm_offset_t)0x20000000;		/* XXX */
	vn.v_type = VCHR;			/* XXX */
	vn.v_specinfo = &si;			/* XXX */
	vn.v_rdev = dev;			/* XXX */
	/*
	 * Map the all the data the user needs access to into
	 * user space.
	 */
	error = vm_mmap(&p->p_vmspace->vm_map, &addr, len,
		VM_PROT_ALL, VM_PROT_ALL, MAP_SHARED, (caddr_t)&vn,
		(vm_offset_t)0);
	if (error)
		return (error);
	fbp = (struct fbuaccess *)(addr + ((vm_offset_t)fp->fbu & PGOFSET));
	*(PM_Info **)data = &fbp->scrInfo;
	fp->fbu->scrInfo.qe.events = fbp->events;
	fp->fbu->scrInfo.qe.tcs = fbp->tcs;
	fp->fbu->scrInfo.planemask = (char *)0;
	/*
	 * Map the frame buffer into the user's address space.
	 */
	fp->fbu->scrInfo.bitmap = (char *)pmax_round_page(fbp + 1);
	return (0);
}
