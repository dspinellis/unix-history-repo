/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dc7085cons.h	7.1 (Berkeley) %G%
 *
 * dc7085.h --
 *
 *     	Definitions for the dc7085 chip.
 *
 *	Copyright (C) 1989 Digital Equipment Corporation.
 *	Permission to use, copy, modify, and distribute this software and
 *	its documentation for any purpose and without fee is hereby granted,
 *	provided that the above copyright notice appears in all copies.
 *	Digital Equipment Corporation makes no representations about the
 *	suitability of this software for any purpose.  It is provided "as is"
 *	without express or implied warranty.
 *
 * from: $Header: /sprite/src/kernel/dev/ds3100.md/RCS/dc7085.h,
 *	v 1.4 89/08/15 19:52:46 rab Exp $ SPRITE (DECWRL)
 */

#ifndef _DC7085
#define _DC7085

typedef volatile struct dc7085regs {
	u_short	dc_csr;		/* control and status (R/W) */
	u_short	pad0[3];
	short	dc_rbuf_lpr;	/* receiver data (R), line params (W) */
	u_short	pad1[3];
	u_short	dc_tcr;		/* transmitter control (R/W) */
	u_short	pad2[3];
	u_short	dc_msr_tdr;	/* modem status (R), transmit data (W) */
} dcregs;
#define dc_rbuf	dc_rbuf_lpr
#define dc_lpr	dc_rbuf_lpr
#define dc_msr	dc_msr_tdr
#define dc_tdr	dc_msr_tdr

/*
 * Control status register bits.
 */
#define	CSR_TRDY	0x8000
#define CSR_TIE		0x4000
#define	CSR_TX_LINE_NUM	0x0300
#define	CSR_RDONE	0x0080
#define	CSR_RIE		0x0040
#define CSR_MSE		0x0020
#define CSR_CLR		0x0010
#define CSR_MAINT	0x0008

/*
 * Receiver buffer register bits.
 */
#define	RBUF_DVAL		0x8000
#define RBUF_OERR		0x4000
#define RBUF_FERR		0x2000
#define RBUF_PERR		0x1000
#define RBUF_LINE_NUM		0x0300
#define RBUF_LINE_NUM_SHIFT	8
#define RBUF_CHAR		0x00FF

/*
 * Transmit control register values.
 */
#define TCR_DTR2		0x400
#define TCR_EN3			0x008
#define TCR_EN2			0x004
#define TCR_EN1			0x002
#define TCR_EN0			0x001

/*
 * Line parameter register bits.
 */
#define	LPR_RXENAB	0x1000
#define LPR_B50		0x0000
#define LPR_B75		0x0100
#define LPR_B110	0x0200
#define LPR_B134	0x0300
#define LPR_B150	0x0400
#define LPR_B300	0x0500
#define LPR_B600	0x0600
#define LPR_B1200	0x0700
#define LPR_B1800	0x0800
#define LPR_B2000	0x0900
#define LPR_B2400	0x0A00
#define LPR_B3600	0x0B00
#define	LPR_B4800	0x0C00
#define LPR_B7200	0x0D00
#define LPR_B9600	0x0E00
#define LPR_B19800	0x0F00
#define LPR_OPAR	0x0080
#define LPR_PARENB	0x0040
#define LPR_2_STOP	0x0020
#define LPR_8_BIT_CHAR	0x0018
#define LPR_7_BIT_CHAR	0x0010
#define LPR_6_BIT_CHAR	0x0008
#define LPR_5_BIT_CHAR	0x0000

/*
 * Modem status register bits.
 */
#define	MSR_DSR2	0x0200

/*
 * The four serial ports.
 */
#define	KBD_PORT	0
#define MOUSE_PORT	1
#define MODEM_PORT	2
#define PRINTER_PORT	3

/*
 * Special key values.
 */
#define KEY_SHIFT	0xae
#define KEY_CONTROL	0xaf
#define KEY_UP		0xb3
#define KEY_REPEAT	0xb4
#define KEY_F1		0x56
#define KEY_COMMAND	KEY_F1

/*
 * Lk201/301 keyboard
 */
#define LK_UPDOWN	0x86		/* bits for setting lk201 modes */
#define LK_AUTODOWN	0x82
#define LK_DOWN		0x80
#define LK_DEFAULTS	0xd3		/* reset mode settings          */
#define LK_AR_ENABLE	0xe3		/* global auto repeat enable	*/
#define LK_CL_ENABLE	0x1b		/* keyclick enable		*/
#define LK_KBD_ENABLE	0x8b		/* keyboard enable		*/
#define LK_BELL_ENABLE	0x23		/* the bell			*/
#define LK_LED_ENABLE	0x13		/* light led			*/
#define LK_LED_DISABLE	0x11		/* turn off led			*/
#define LK_RING_BELL	0xa7		/* ring keyboard bell		*/
#define LED_1		0x81		/* led bits			*/
#define LED_2		0x82
#define LED_3		0x84
#define LED_4		0x88
#define LED_ALL		0x8f
#define LK_HELP		0x7c		/* help key			*/
#define LK_DO		0x7d		/* do key			*/
#define LK_KDOWN_ERROR	0x3d		/* key down on powerup error	*/
#define LK_POWER_ERROR	0x3e		/* keyboard failure on pwrup tst*/
#define LK_OUTPUT_ERROR 0xb5		/* keystrokes lost during inhbt */
#define LK_INPUT_ERROR	0xb6		/* garbage command to keyboard	*/
#define LK_LOWEST	0x56		/* lowest significant keycode	*/

/* max volume is 0, lowest is 0x7 */
#define	LK_PARAM_VOLUME(v)		(0x80|((v)&0x7))

/* mode command details */
#define	LK_CMD_MODE(m,div)		((m)|((div)<<3))

/*
 * Command characters for the mouse.
 */
#define MOUSE_SELF_TEST		'T'
#define MOUSE_INCREMENTAL	'R'

/*
 * Mouse output bits.
 *
 *     	MOUSE_START_FRAME	Start of report frame bit.
 *	MOUSE_X_SIGN		Sign bit for X.
 *	MOUSE_Y_SIGN		Sign bit for Y.
 *	MOUSE_X_OFFSET		X offset to start cursor at.
 *	MOUSE_Y_OFFSET		Y offset to start cursor at.
 */
#define MOUSE_START_FRAME	0x80
#define MOUSE_X_SIGN		0x10
#define MOUSE_Y_SIGN		0x08

/*
 * Definitions for mouse buttons
 */
#define EVENT_LEFT_BUTTON	0x01
#define EVENT_MIDDLE_BUTTON	0x02
#define EVENT_RIGHT_BUTTON	0x03
#define RIGHT_BUTTON		0x01
#define MIDDLE_BUTTON		0x02
#define LEFT_BUTTON		0x04

/*
 * Mouse report structure definition
 */
typedef struct {
	char state;			/* buttons and sign bits	*/
	short dx;			/* delta X since last change	*/
	short dy;			/* delta Y since last change	*/
	char byteCount;			/* mouse report byte count	*/
} MouseReport;

/* bits in dm lsr, copied from dmreg.h */
#define	DML_DSR		0000400		/* data set ready, not a real DM bit */
#define	DML_RNG		0000200		/* ring */
#define	DML_CAR		0000100		/* carrier detect */
#define	DML_CTS		0000040		/* clear to send */
#define	DML_SR		0000020		/* secondary receive */
#define	DML_ST		0000010		/* secondary transmit */
#define	DML_RTS		0000004		/* request to send */
#define	DML_DTR		0000002		/* data terminal ready */
#define	DML_LE		0000001		/* line enable */

#endif /* _DC7085 */
