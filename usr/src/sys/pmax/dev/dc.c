/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *	@(#)dz.c	7.9 (Berkeley) 6/28/90
 */

/*
 *  devDC7085.c --
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dc.c	7.7 (Berkeley) %G%
 *
 * devDC7085.c --
 *
 *     	This file contains machine-dependent routines that handle the
 *	output queue for the serial lines.
 *
 *	Copyright (C) 1989 Digital Equipment Corporation.
 *	Permission to use, copy, modify, and distribute this software and
 *	its documentation for any purpose and without fee is hereby granted,
 *	provided that the above copyright notice appears in all copies.
 *	Digital Equipment Corporation makes no representations about the
 *	suitability of this software for any purpose.  It is provided "as is"
 *	without express or implied warranty.
 *
 * from: $Header: /sprite/src/kernel/dev/ds3100.md/RCS/devDC7085.c,
 *	v 1.4 89/08/29 11:55:30 nelson Exp $ SPRITE (DECWRL)";
 */

#include "dc.h"
#if NDC > 0
/*
 * DC7085 (DZ-11 look alike) Driver
 */
#include "param.h"
#include "systm.h"
#include "ioctl.h"
#include "tty.h"
#include "proc.h"
#include "map.h"
#include "buf.h"
#include "conf.h"
#include "file.h"
#include "uio.h"
#include "kernel.h"
#include "syslog.h"

#include "machine/dc7085cons.h"

#include "device.h"
#include "pdma.h"

/*
 * Driver information for auto-configuration stuff.
 */
int	dcprobe();
void	dcintr();
struct	driver dcdriver = {
	"dc", dcprobe, 0, 0, dcintr,
};

#define	NDCLINE 	(NDC*4)

extern void dcstart __P((struct tty *));
extern void dcxint __P((struct tty *));
extern void ttrstrt __P((struct tty *));

struct	tty dc_tty[NDCLINE];
int	dc_cnt = NDCLINE;
void	(*dcDivertXInput)();	/* X windows keyboard input routine */
void	(*dcMouseEvent)();	/* X windows mouse motion event routine */
void	(*dcMouseButtons)();	/* X windows mouse buttons event routine */
#ifdef DEBUG
int	debugChar;
#endif

static void dcscan __P((void *));
static int dcMapChar __P((int));
static void dcKBDReset __P((void));
static void MouseInit __P((void));

/*
 * Software copy of brk register since it isn't readable
 */
int	dc_brk[NDC];
char	dcsoftCAR[NDC];		/* mask of dc's with carrier on (DSR) */

/*
 * The DC7085 doesn't interrupt on carrier transitions, so
 * we have to use a timer to watch it.
 */
int	dc_timer;		/* true if timer started */

/*
 * Pdma structures for fast output code
 */
struct	pdma dcpdma[NDCLINE];

struct speedtab dcspeedtab[] = {
	0,	0,
	50,	LPR_B50,
	75,	LPR_B75,
	110,	LPR_B110,
	134,	LPR_B134,
	150,	LPR_B150,
	300,	LPR_B300,
	600,	LPR_B600,
	1200,	LPR_B1200,
	1800,	LPR_B1800,
	2400,	LPR_B2400,
	4800,	LPR_B4800,
	9600,	LPR_B9600,
#ifdef DS5000
	19200,	LPR_B19200,
#endif
	-1,	-1
};

#ifndef	PORTSELECTOR
#define	ISPEED	TTYDEF_SPEED
#define	LFLAG	TTYDEF_LFLAG
#else
#define	ISPEED	B4800
#define	LFLAG	(TTYDEF_LFLAG & ~ECHO)
#endif

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
 * Test to see if device is present.
 * Return true if found and initialized ok.
 */
dcprobe(cp)
	register struct pmax_ctlr *cp;
{
	register dcregs *dcaddr;
	register struct pdma *pdp;
	register struct tty *tp;
	register int cntr;

	if (cp->pmax_unit >= NDC)
		return (0);
	if (badaddr(cp->pmax_addr, 2))
		return (0);

	/* reset chip */
	dcaddr = (dcregs *)cp->pmax_addr;
	dcaddr->dc_csr = CSR_CLR;
	MachEmptyWriteBuffer();
	while (dcaddr->dc_csr & CSR_CLR)
		;
	dcaddr->dc_csr = CSR_MSE | CSR_TIE | CSR_RIE;

	/* init pseudo DMA structures */
	pdp = &dcpdma[cp->pmax_unit * 4];
	tp = &dc_tty[cp->pmax_unit * 4];
	for (cntr = 0; cntr < 4; cntr++) {
		pdp->p_addr = dcaddr;
		pdp->p_arg = (int)tp;
		pdp->p_fcn = dcxint;
		tp->t_addr = (caddr_t)pdp;
		pdp++, tp++;
	}
	dcsoftCAR[cp->pmax_unit] = cp->pmax_flags | 0xB;

	if (dc_timer == 0) {
		dc_timer = 1;
		timeout(dcscan, (void *)0, hz);
	}
	printf("dc%d at nexus0 csr 0x%x priority %d\n",
		cp->pmax_unit, cp->pmax_addr, cp->pmax_pri);
	if (cp->pmax_unit == 0) {
		int s;

		s = spltty();
		dcaddr->dc_lpr = LPR_RXENAB | LPR_B4800 | LPR_8_BIT_CHAR |
			KBD_PORT;
		dcaddr->dc_lpr = LPR_RXENAB | LPR_B4800 | LPR_OPAR |
			LPR_PARENB | LPR_8_BIT_CHAR | MOUSE_PORT;
		MachEmptyWriteBuffer();
		dcKBDReset();
		MouseInit();
		splx(s);
	}
	return (1);
}

dcopen(dev, flag, mode, p)
	dev_t dev;
	int flag, mode;
	struct proc *p;
{
	register struct tty *tp;
	register int unit;
	int s, error = 0;
	extern int dcparam();

	unit = minor(dev);
	if (unit >= dc_cnt || dcpdma[unit].p_addr == 0)
		return (ENXIO);
	tp = &dc_tty[unit];
	tp->t_addr = (caddr_t)&dcpdma[unit];
	tp->t_oproc = dcstart;
	tp->t_param = dcparam;
	tp->t_dev = dev;
	if ((tp->t_state & TS_ISOPEN) == 0) {
		tp->t_state |= TS_WOPEN;
		ttychars(tp);
#ifndef PORTSELECTOR
		if (tp->t_ispeed == 0) {
#endif
			tp->t_iflag = TTYDEF_IFLAG;
			tp->t_oflag = TTYDEF_OFLAG;
			tp->t_cflag = TTYDEF_CFLAG;
			tp->t_lflag = LFLAG;
			tp->t_ispeed = tp->t_ospeed = ISPEED;
#ifdef PORTSELECTOR
			tp->t_cflag |= HUPCL;
#else
		}
#endif
		(void) dcparam(tp, &tp->t_termios);
		ttsetwater(tp);
	} else if ((tp->t_state & TS_XCLUDE) && curproc->p_ucred->cr_uid != 0)
		return (EBUSY);
	(void) dcmctl(dev, DML_DTR, DMSET);
	s = spltty();
	while (!(flag & O_NONBLOCK) && !(tp->t_cflag & CLOCAL) &&
	       !(tp->t_state & TS_CARR_ON)) {
		tp->t_state |= TS_WOPEN;
		if (error = ttysleep(tp, (caddr_t)&tp->t_rawq, TTIPRI | PCATCH,
		    ttopen, 0))
			break;
	}
	splx(s);
	if (error)
		return (error);
	return ((*linesw[tp->t_line].l_open)(dev, tp));
}

/*ARGSUSED*/
dcclose(dev, flag, mode, p)
	dev_t dev;
	int flag, mode;
	struct proc *p;
{
	register struct tty *tp;
	register int unit, bit;

	unit = minor(dev);
	tp = &dc_tty[unit];
	bit = 1 << ((unit & 03) + 8);
	if (dc_brk[unit >> 2] & bit) {
		dc_brk[unit >> 2] &= ~bit;
		ttyoutput(0, tp);
	}
	(*linesw[tp->t_line].l_close)(tp, flag);
	if ((tp->t_cflag & HUPCL) || (tp->t_state & TS_WOPEN) ||
	    !(tp->t_state & TS_ISOPEN))
		(void) dcmctl(dev, 0, DMSET);
	return (ttyclose(tp));
}

dcread(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp;

	tp = &dc_tty[minor(dev)];
	return ((*linesw[tp->t_line].l_read)(tp, uio, flag));
}

dcwrite(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp;

	tp = &dc_tty[minor(dev)];
	return ((*linesw[tp->t_line].l_write)(tp, uio, flag));
}

/*ARGSUSED*/
dcioctl(dev, cmd, data, flag, p)
	dev_t dev;
	caddr_t data;
	int flag;
	struct proc *p;
{
	register struct tty *tp;
	register int unit = minor(dev);
	register int dc = unit >> 2;
	int error;

	tp = &dc_tty[unit];
	error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, data, flag, p);
	if (error >= 0)
		return (error);
	error = ttioctl(tp, cmd, data, flag);
	if (error >= 0)
		return (error);

	switch (cmd) {

	case TIOCSBRK:
		dc_brk[dc] |= 1 << ((unit & 03) + 8);
		ttyoutput(0, tp);
		break;

	case TIOCCBRK:
		dc_brk[dc] &= ~(1 << ((unit & 03) + 8));
		ttyoutput(0, tp);
		break;

	case TIOCSDTR:
		(void) dcmctl(dev, DML_DTR|DML_RTS, DMBIS);
		break;

	case TIOCCDTR:
		(void) dcmctl(dev, DML_DTR|DML_RTS, DMBIC);
		break;

	case TIOCMSET:
		(void) dcmctl(dev, *(int *)data, DMSET);
		break;

	case TIOCMBIS:
		(void) dcmctl(dev, *(int *)data, DMBIS);
		break;

	case TIOCMBIC:
		(void) dcmctl(dev, *(int *)data, DMBIC);
		break;

	case TIOCMGET:
		*(int *)data = dcmctl(dev, 0, DMGET);
		break;

	default:
		return (ENOTTY);
	}
	return (0);
}

dcparam(tp, t)
	register struct tty *tp;
	register struct termios *t;
{
	register dcregs *dcaddr;
	register int lpr;
	register int cflag = t->c_cflag;
	int unit = minor(tp->t_dev);
	int ospeed = ttspeedtab(t->c_ospeed, dcspeedtab);

	/* check requested parameters */
        if (ospeed < 0 || (t->c_ispeed && t->c_ispeed != t->c_ospeed) ||
            (cflag & CSIZE) == CS5 || (cflag & CSIZE) == CS6)
                return (EINVAL);
        /* and copy to tty */
        tp->t_ispeed = t->c_ispeed;
        tp->t_ospeed = t->c_ospeed;
        tp->t_cflag = cflag;

	dcaddr = dcpdma[unit].p_addr;
	if (tp == dc_tty + KBD_PORT) {
		/* handle the keyboard specially */
		dcaddr->dc_lpr = LPR_RXENAB | LPR_B4800 | LPR_8_BIT_CHAR |
			KBD_PORT;
		MachEmptyWriteBuffer();
		return (0);
	}
	if (tp == dc_tty + MOUSE_PORT) {
		/* handle the mouse specially */
		dcaddr->dc_lpr = LPR_RXENAB | LPR_B4800 | LPR_OPAR |
			LPR_PARENB | LPR_8_BIT_CHAR | MOUSE_PORT;
		MachEmptyWriteBuffer();
		return (0);
	}
	if (ospeed == 0) {
		(void) dcmctl(unit, 0, DMSET);	/* hang up line */
		return (0);
	}
	lpr = LPR_RXENAB | ospeed | (unit & 03);
	if ((cflag & CSIZE) == CS7)
		lpr |= LPR_7_BIT_CHAR;
	else
		lpr |= LPR_8_BIT_CHAR;
	if (cflag & PARENB)
		lpr |= LPR_PARENB;
	if (cflag & PARODD)
		lpr |= LPR_OPAR;
	if (cflag & CSTOPB)
		lpr |= LPR_2_STOP;
	dcaddr->dc_lpr = lpr;
	MachEmptyWriteBuffer();
	return (0);
}

/*
 * Check for interrupts from all devices.
 */
void
dcintr(unit)
	register int unit;
{
	register dcregs *dcaddr;
	register unsigned csr;

	unit <<= 2;
	dcaddr = dcpdma[unit].p_addr;
	while ((csr = dcaddr->dc_csr) & (CSR_RDONE | CSR_TRDY)) {
		if (csr & CSR_RDONE)
			dcrint(unit);
		if (csr & CSR_TRDY)
			dcxint(&dc_tty[unit + ((csr >> 8) & 03)]);
	}
}

dcrint(unit)
	register int unit;
{
	register dcregs *dcaddr;
	register struct tty *tp;
	register int c, cc;
	register struct tty *tp0;
	int overrun = 0;

	dcaddr = dcpdma[unit].p_addr;
	tp0 = &dc_tty[unit];
	while ((c = dcaddr->dc_rbuf) < 0) {	/* char present */
		cc = c & 0xff;
		tp = tp0 + ((c >> 8) & 03);
		if ((c & RBUF_OERR) && overrun == 0) {
			log(LOG_WARNING, "dc%d,%d: silo overflow\n", unit >> 2,
				(c >> 8) & 03);
			overrun = 1;
		}
		/* the keyboard requires special translation */
		if (tp == &dc_tty[KBD_PORT]) {
#ifdef KADB
			if (cc == LK_DO) {
				spl0();
				kdbpanic();
				return;
			}
#endif
#ifdef DEBUG
			debugChar = cc;
#endif
			if (dcDivertXInput) {
				(*dcDivertXInput)(cc);
				return;
			}
			if ((cc = dcMapChar(cc)) < 0)
				return;
		} else if (tp == &dc_tty[MOUSE_PORT] && dcMouseButtons) {
			register MouseReport *mrp;
			static MouseReport currentRep;

			mrp = &currentRep;
			mrp->byteCount++;
			if (cc & MOUSE_START_FRAME) {
				/*
				 * The first mouse report byte (button state).
				 */
				mrp->state = cc;
				if (mrp->byteCount > 1)
					mrp->byteCount = 1;
			} else if (mrp->byteCount == 2) {
				/*
				 * The second mouse report byte (delta x).
				 */
				mrp->dx = cc;
			} else if (mrp->byteCount == 3) {
				/*
				 * The final mouse report byte (delta y).
				 */
				mrp->dy = cc;
				mrp->byteCount = 0;
				if (mrp->dx != 0 || mrp->dy != 0) {
					/*
					 * If the mouse moved,
					 * post a motion event.
					 */
					(*dcMouseEvent)(mrp);
				}
				(*dcMouseButtons)(mrp);
			}
			return;
		}
		if (!(tp->t_state & TS_ISOPEN)) {
			wakeup((caddr_t)&tp->t_rawq);
#ifdef PORTSELECTOR
			if (!(tp->t_state & TS_WOPEN))
#endif
				return;
		}
		if (c & RBUF_FERR)
			cc |= TTY_FE;
		if (c & RBUF_PERR)
			cc |= TTY_PE;
		(*linesw[tp->t_line].l_rint)(cc, tp);
	}
	DELAY(10);
}

void
dcxint(tp)
	register struct tty *tp;
{
	register struct pdma *dp;
	register dcregs *dcaddr;

	dp = (struct pdma *)tp->t_addr;
	if (dp->p_mem < dp->p_end) {
		dcaddr = dp->p_addr;
		dcaddr->dc_tdr = dc_brk[(tp - dc_tty) >> 2] | *dp->p_mem++;
		MachEmptyWriteBuffer();
		DELAY(10);
		return;
	}
	tp->t_state &= ~TS_BUSY;
	if (tp->t_state & TS_FLUSH)
		tp->t_state &= ~TS_FLUSH;
	else {
		ndflush(&tp->t_outq, dp->p_mem-tp->t_outq.c_cf);
		dp->p_end = dp->p_mem = tp->t_outq.c_cf;
	}
	if (tp->t_line)
		(*linesw[tp->t_line].l_start)(tp);
	else
		dcstart(tp);
	if (tp->t_outq.c_cc == 0 || !(tp->t_state & TS_BUSY)) {
		dp->p_addr->dc_tcr &= ~(1 << (minor(tp->t_dev) & 03));
		MachEmptyWriteBuffer();
		DELAY(10);
	}
}

void
dcstart(tp)
	register struct tty *tp;
{
	register struct pdma *dp;
	register dcregs *dcaddr;
	register int cc;
	int s;

	dp = (struct pdma *)tp->t_addr;
	dcaddr = dp->p_addr;
	s = spltty();
	if (tp->t_state & (TS_TIMEOUT|TS_BUSY|TS_TTSTOP))
		goto out;
	if (tp->t_outq.c_cc <= tp->t_lowat) {
		if (tp->t_state & TS_ASLEEP) {
			tp->t_state &= ~TS_ASLEEP;
			wakeup((caddr_t)&tp->t_outq);
		}
		selwakeup(&tp->t_wsel);
	}
	if (tp->t_outq.c_cc == 0)
		goto out;
	/* handle console specially */
	if (tp == dc_tty) {
		while (tp->t_outq.c_cc > 0) {
			cc = getc(&tp->t_outq) & 0x7f;
			cnputc(cc);
		}
		/*
		 * After we flush the output queue we may need to wake
		 * up the process that made the output.
		 */
		if (tp->t_outq.c_cc <= tp->t_lowat) {
			if (tp->t_state & TS_ASLEEP) {
				tp->t_state &= ~TS_ASLEEP;
				wakeup((caddr_t)&tp->t_outq);
			}
			selwakeup(&tp->t_wsel);
		}
		goto out;
	}
	if (tp->t_flags & (RAW|LITOUT))
		cc = ndqb(&tp->t_outq, 0);
	else {
		cc = ndqb(&tp->t_outq, 0200);
		if (cc == 0) {
			cc = getc(&tp->t_outq);
			timeout(ttrstrt, (void *)tp, (cc & 0x7f) + 6);
			tp->t_state |= TS_TIMEOUT;
			goto out;
		}
	}
	tp->t_state |= TS_BUSY;
	dp->p_end = dp->p_mem = tp->t_outq.c_cf;
	dp->p_end += cc;
	dcaddr->dc_tcr |= 1 << (minor(tp->t_dev) & 03);
	MachEmptyWriteBuffer();
out:
	splx(s);
}

/*
 * Stop output on a line.
 */
/*ARGSUSED*/
dcstop(tp, flag)
	register struct tty *tp;
{
	register struct pdma *dp;
	register int s;

	dp = (struct pdma *)tp->t_addr;
	s = spltty();
	if (tp->t_state & TS_BUSY) {
		dp->p_end = dp->p_mem;
		if (!(tp->t_state & TS_TTSTOP))
			tp->t_state |= TS_FLUSH;
	}
	splx(s);
}

dcmctl(dev, bits, how)
	dev_t dev;
	int bits, how;
{
	register dcregs *dcaddr;
	register int unit, mbits;
	int b, s;
#ifdef DS5000
	register int msr;
#endif

	unit = minor(dev);
	b = 1 << (unit & 03);
	dcaddr = dcpdma[unit].p_addr;
	s = spltty();
	/* only channel 2 has modem control (what about line 3?) */
	switch (unit & 03) {
	case 2:
		mbits = 0;
		if (dcaddr->dc_tcr & TCR_DTR2)
			mbits |= DML_DTR;
#ifdef DS3100
		if (dcaddr->dc_msr & MSR_DSR2)
			mbits |= DML_DSR | DML_CAR;
#endif
#ifdef DS5000
		msr = dcaddr->dc_msr;
		if (msr & MSR_CD2)
			mbits |= DML_CAR;
		if (msr & MSR_DSR2)
			mbits |= DML_DSR;
#endif
		break;

#ifdef DS5000
	case 3:
		mbits = 0;
		if (dcaddr->dc_tcr & TCR_DTR3)
			mbits |= DML_DTR;
		msr = dcaddr->dc_msr;
		if (msr & MSR_CD3)
			mbits |= DML_CAR;
		if (msr & MSR_DSR3)
			mbits |= DML_DSR;
		break;
#endif

	default:
		mbits = DML_DTR | DML_DSR | DML_CAR;
	}
	switch (how) {
	case DMSET:
		mbits = bits;
		break;

	case DMBIS:
		mbits |= bits;
		break;

	case DMBIC:
		mbits &= ~bits;
		break;

	case DMGET:
		(void) splx(s);
		return (mbits);
	}
	switch (unit & 03) {
	case 2:
		if (mbits & DML_DTR)
			dcaddr->dc_tcr |= TCR_DTR2;
		else
			dcaddr->dc_tcr &= ~TCR_DTR2;
		break;

#ifdef DS5000
	case 3:
		if (mbits & DML_DTR)
			dcaddr->dc_tcr |= TCR_DTR3;
		else
			dcaddr->dc_tcr &= ~TCR_DTR3;
#endif
	}
	if ((mbits & DML_DTR) && (dcsoftCAR[unit >> 2] & b))
		dc_tty[unit].t_state |= TS_CARR_ON;
	(void) splx(s);
	return (mbits);
}

/*
 * This is called by timeout() periodically.
 * Check to see if modem status bits have changed.
 */
/* ARGSUSED */
static void
dcscan(arg)
	void *arg;
{
	register dcregs *dcaddr;
	register struct tty *tp;
	register int i, bit, car;
	int s;

	s = spltty();
	/* only channel 2 has modem control (what about line 3?) */
	dcaddr = dcpdma[i = 2].p_addr;
	tp = &dc_tty[i];
	bit = TCR_DTR2;
	if (dcsoftCAR[i >> 2] & bit)
		car = 1;
	else
		car = dcaddr->dc_msr & MSR_DSR2;
	if (car) {
		/* carrier present */
		if (!(tp->t_state & TS_CARR_ON))
			(void)(*linesw[tp->t_line].l_modem)(tp, 1);
	} else if ((tp->t_state & TS_CARR_ON) &&
	    (*linesw[tp->t_line].l_modem)(tp, 0) == 0)
		dcaddr->dc_tcr &= ~bit;
	splx(s);
	timeout(dcscan, (void *)0, hz);
}

/*
 * ----------------------------------------------------------------------------
 *
 * dcKBDPutc --
 *
 *	Put a character out to the keyboard.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	A character is written to the keyboard.
 *
 * ----------------------------------------------------------------------------
 */
void
dcKBDPutc(c)
	register int c;
{
	register dcregs *dcaddr;
	register u_short tcr;
	register int timeout;
	int s, line;

	s = spltty();

	dcaddr = dcpdma[KBD_PORT].p_addr;
	tcr = dcaddr->dc_tcr;
	dcaddr->dc_tcr = tcr | (1 << KBD_PORT);
	MachEmptyWriteBuffer();
	DELAY(10);
	while (1) {
		/*
		 * Wait for transmitter to be not busy.
		 */
		timeout = 1000000;
		while (!(dcaddr->dc_csr & CSR_TRDY) && timeout > 0)
			timeout--;
		if (timeout == 0) {
			printf("dcKBDPutc: timeout waiting for CSR_TRDY\n");
			break;
		}
		line = (dcaddr->dc_csr >> 8) & 3;
		/*
		 * Check to be sure its the right port.
		 */
		if (line != KBD_PORT) {
			tcr |= 1 << line;
			dcaddr->dc_tcr &= ~(1 << line);
			MachEmptyWriteBuffer();
			DELAY(10);
			continue;
		}
		/*
		 * Start sending the character.
		 */
		dcaddr->dc_tdr = dc_brk[0] | (c & 0xff);
		MachEmptyWriteBuffer();
		DELAY(10);
		/*
		 * Wait for character to be sent.
		 */
		while (1) {
			/*
			 * cc -O bug: this code produces and infinite loop!
			 * while (!(dcaddr->dc_csr & CSR_TRDY))
			 *	;
			 */
			timeout = 1000000;
			while (!(dcaddr->dc_csr & CSR_TRDY) && timeout > 0)
				timeout--;
			line = (dcaddr->dc_csr >> 8) & 3;
			if (line != KBD_PORT) {
				tcr |= 1 << line;
				dcaddr->dc_tcr &= ~(1 << line);
				MachEmptyWriteBuffer();
				DELAY(10);
				continue;
			}
			dcaddr->dc_tcr &= ~(1 << KBD_PORT);
			MachEmptyWriteBuffer();
			DELAY(10);
			break;
		}
		break;
	}
	/*
	 * Enable interrupts for other lines which became ready.
	 */
	if (tcr & 0xF) {
		dcaddr->dc_tcr = tcr;
		MachEmptyWriteBuffer();
		DELAY(10);
	}

	splx(s);
}

#ifdef DEBUG
/*
 * ----------------------------------------------------------------------------
 *
 * dcDebugGetc --
 *
 *	Read a character from the keyboard if one is ready (i.e., don't wait).
 *
 * Results:
 *	A character read from the mouse, -1 if none were ready.
 *
 * Side effects:
 *	None.
 *
 * ----------------------------------------------------------------------------
 */
int
dcDebugGetc()
{
	register dcregs *dcaddr;
	register int c;
	int s;

	dcaddr = dcpdma[KBD_PORT].p_addr;
	if (!dcaddr)
		return (0);

	s = spltty();
	if (c = debugChar)
		debugChar = 0;
	else {
		while (dcaddr->dc_csr & CSR_RDONE) {
			c = dcaddr->dc_rbuf;
			DELAY(10);
			if (((c >> 8) & 03) == KBD_PORT)
				break;
			c = 0;
		}
	}
	splx(s);

	return (c & 0xff);
}
#endif

/*
 * ----------------------------------------------------------------------------
 *
 * dcKBDGetc --
 *
 *	Read a character from the keyboard.
 *
 * Results:
 *	A character read from the keyboard.
 *
 * Side effects:
 *	None.
 *
 * ----------------------------------------------------------------------------
 */
int
dcKBDGetc()
{
	register dcregs *dcaddr;
	register int c;
	int s;

	dcaddr = dcpdma[KBD_PORT].p_addr;
	if (!dcaddr)
		return (-1);
	s = spltty();
	for (;;) {
		if (!(dcaddr->dc_csr & CSR_RDONE))
			continue;
		c = dcaddr->dc_rbuf;
		DELAY(10);
		if (((c >> 8) & 03) != KBD_PORT)
			continue;
		if ((c = dcMapChar(c & 0xff)) >= 0)
			break;
	}
	splx(s);
	return (c);
}

/*
 * ----------------------------------------------------------------------------
 *
 * dcMapChar --
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
static int
dcMapChar(cc)
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
		if (ctrlDown)
			shiftDown = 0;
		else
			shiftDown = 1;
		return (-1);

	case KEY_CONTROL:
		if (shiftDown)
			ctrlDown = 0;
		else
			ctrlDown = 1;
		return (-1);

	case LK_POWER_ERROR:
	case LK_KDOWN_ERROR:
	case LK_INPUT_ERROR:
	case LK_OUTPUT_ERROR:
		log(LOG_WARNING,
			"dc0,0: keyboard error, code=%x\n", cc);
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
 * ----------------------------------------------------------------------------
 *
 * dcKBDReset --
 *
 *	Reset the keyboard to default characteristics.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 * ----------------------------------------------------------------------------
 */
void
dcKBDReset()
{
	register int i;
	static int inKBDReset;

	if (inKBDReset)
		return;
	inKBDReset = 1;
	for (i = 0; i < sizeof(kbdInitString); i++)
		dcKBDPutc((int)kbdInitString[i]);
	inKBDReset = 0;
}

/*
 * ----------------------------------------------------------------------------
 *
 * MousePutc --
 *
 *	Write a character to the mouse.
 *	This is only called at initialization time.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	A character is written to the mouse.
 *
 * ----------------------------------------------------------------------------
 */
static void
MousePutc(c)
	int c;
{
	register dcregs *dcaddr;
	register u_short tcr;
	register int timeout;
	int line;

	dcaddr = dcpdma[MOUSE_PORT].p_addr;
	tcr = dcaddr->dc_tcr;
	dcaddr->dc_tcr = tcr | (1 << MOUSE_PORT);
	MachEmptyWriteBuffer();
	DELAY(10);
	while (1) {
		/*
		 * Wait for transmitter to be not busy.
		 */
		timeout = 1000000;
		while (!(dcaddr->dc_csr & CSR_TRDY) && timeout > 0)
			timeout--;
		if (timeout == 0) {
			printf("MousePutc: timeout waiting for CSR_TRDY\n");
			break;
		}
		line = (dcaddr->dc_csr >> 8) & 3;
		/*
		 * Check to be sure its the right port.
		 */
		if (line != MOUSE_PORT) {
			tcr |= 1 << line;
			dcaddr->dc_tcr &= ~(1 << line);
			MachEmptyWriteBuffer();
			DELAY(10);
			continue;
		}
		/*
		 * Start sending the character.
		 */
		dcaddr->dc_tdr = dc_brk[0] | (c & 0xff);
		MachEmptyWriteBuffer();
		DELAY(10);
		/*
		 * Wait for character to be sent.
		 */
		while (1) {
			/*
			 * cc -O bug: this code produces and infinite loop!
			 * while (!(dcaddr->dc_csr & CSR_TRDY))
			 *	;
			 */
			timeout = 1000000;
			while (!(dcaddr->dc_csr & CSR_TRDY) && timeout > 0)
				timeout--;
			line = (dcaddr->dc_csr >> 8) & 3;
			if (line != MOUSE_PORT) {
				tcr |= 1 << line;
				dcaddr->dc_tcr &= ~(1 << line);
				MachEmptyWriteBuffer();
				DELAY(10);
				continue;
			}
			dcaddr->dc_tcr &= ~(1 << MOUSE_PORT);
			MachEmptyWriteBuffer();
			DELAY(10);
			break;
		}
		break;
	}
	/*
	 * Enable interrupts for other lines which became ready.
	 */
	if (tcr & 0xF) {
		dcaddr->dc_tcr = tcr;
		MachEmptyWriteBuffer();
		DELAY(10);
	}
}

/*
 * ----------------------------------------------------------------------------
 *
 * MouseGetc --
 *
 *	Read a character from the mouse.
 *	This is only called at initialization time.
 *
 * Results:
 *	A character read from the mouse, -1 if we timed out waiting.
 *
 * Side effects:
 *	None.
 *
 * ----------------------------------------------------------------------------
 */
static int
MouseGetc()
{
	register dcregs *dcaddr;
	register int timeout;
	register int c;

	dcaddr = dcpdma[MOUSE_PORT].p_addr;
	for (timeout = 1000000; timeout > 0; timeout--) {
		if (!(dcaddr->dc_csr & CSR_RDONE))
			continue;
		c = dcaddr->dc_rbuf;
		DELAY(10);
		if (((c >> 8) & 03) != MOUSE_PORT)
			continue;
		return (c & 0xff);
	}

	return (-1);
}

/*
 * ----------------------------------------------------------------------------
 *
 * MouseInit --
 *
 *	Initialize the mouse.
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
MouseInit()
{
	int id_byte1, id_byte2, id_byte3, id_byte4;

	/*
	 * Initialize the mouse.
	 */
	MousePutc(MOUSE_SELF_TEST);
	id_byte1 = MouseGetc();
	if (id_byte1 < 0) {
		printf("MouseInit: Timeout on 1st byte of self-test report\n");
		return;
	}
	id_byte2 = MouseGetc();
	if (id_byte2 < 0) {
		printf("MouseInit: Timeout on 2nd byte of self-test report\n");
		return;
	}
	id_byte3 = MouseGetc();
	if (id_byte3 < 0) {
		printf("MouseInit: Timeout on 3rd byte of self-test report\n");
		return;
	}
	id_byte4 = MouseGetc();
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
	MousePutc(MOUSE_INCREMENTAL);
}
#endif /* NDC */
