/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kbms_if.c	7.3 (Berkeley) %G%
 */

/* Keyboard Mouse Gate-array control routine */

#include <machine/adrsmap.h>

#include "ms.h"
#include "bm.h"

#include <sys/param.h>
#include <news3400/sio/scc.h>

#if defined(news3200)
#include <sys/time.h>
#include <news3400/iop/mouse.h>
#endif

typedef struct kbm_sw {
	u_char *stat_port;	/* Status port */
	u_char *data_port;	/* Data port */
	u_char *intr_port;	/* Interrupt port */
	u_char *reset_port;	/* Reset port */
	u_char *init1_port;	/* Initialize port 1 */
	u_char *init2_port;	/* Initialize port 2 */
	u_char *buzz_port;	/* Buzzer port */
	u_char *buzzf_port;	/* Buzzer frequency port */
	u_char intr_en;		/* Data for Interrupt Enable */
	u_char intr_in;		/* Interrupt Occur flag */
	u_char data_rdy;	/* Data Ready flag */
	u_char init1;		/* Speed */
	u_char init2;		/* Clock */
	u_char buzzf;		/* Buzzer frequency */
} Kbm_sw;

#define OFF 0x80
struct kbm_sw Kbm_port[] = {
	{
#ifdef news3400
		(u_char *) MOUSE_STAT,
#else
		(u_char *) KEYB_STAT,
#endif
		(u_char *) MOUSE_DATA,
		(u_char *) MOUSE_INTE,
		(u_char *) MOUSE_RESET,
		(u_char *) MOUSE_INIT1,
		(u_char *) MOUSE_INIT2,
		(u_char *) KEYB_BUZZ,
		(u_char *) KEYB_BUZZF,
		RX_MSINTE,
		RX_MSINT,
		RX_MSRDY,
#ifdef news3400
		0x80,	/* 1200 bps */
		0,
		0
#else
		1,
		0xe0,
		0x0a
#endif
	},
	{
		(u_char *) KEYB_STAT,
		(u_char *) KEYB_DATA,
		(u_char *) KEYB_INTE,
		(u_char *) KEYB_RESET,
		(u_char *) KEYB_INIT1,
		(u_char *) KEYB_INIT2,
		(u_char *) KEYB_BUZZ,
		(u_char *) KEYB_BUZZF,
		RX_KBINTE,
		RX_KBINT,
		RX_KBRDY,
#ifdef news3400
		0xf0,	/* 9600 bps */
		0,
		0
#else
		0,
		0xc0,
		0x0a
#endif
	}
};

kbm_open(chan)
	int chan;
{
	register Kbm_sw *kbm = &Kbm_port[chan];

#ifdef news3400
	/*
	 * Reset KB I/F.
	 * Disable KB interrupt.
	 * Clear KB overrun flag.
	 */
	*(volatile u_char *)kbm->reset_port = (u_char)0x01;
	*(volatile u_char *)kbm->init1_port = kbm->init1;
	if (chan == SCC_MOUSE)
		*(volatile u_char *)kbm->intr_port |= kbm->intr_en;
#else
	*kbm->reset_port = (u_char)0;
	*kbm->intr_port = (u_char)1;
#endif
	kbd_flush();
}

kbm_close(chan)
	int chan;
{
	register Kbm_sw *kbm = &Kbm_port[chan];

#ifdef news3400
	*(volatile u_char *)kbm->reset_port = (u_char)0x01;
#else
	*kbm->reset_port = (u_char)0;
	*kbm->intr_port = (u_char)0;
#endif
}

kbm_rint(chan)
	int chan;
{
#ifdef news3400
	volatile u_char *port = (volatile u_char *)Kbm_port[chan].data_port;
	volatile u_char *stat = (volatile u_char *)Kbm_port[chan].stat_port;
	volatile u_char *inte = (volatile u_char *)Kbm_port[chan].intr_port;
#else
	register u_char *port = Kbm_port[chan].data_port;
	register u_char *stat = Kbm_port[chan].stat_port;
	register u_char *inte = Kbm_port[chan].intr_port;
#endif
	int rdy = Kbm_port[chan].data_rdy;
	u_char code;

#ifdef news3400
	*inte &= ~Kbm_port[chan].intr_en;
#endif

	while (*stat & rdy) {
		code = *port;
		switch (chan) {
		    case SCC_MOUSE: {
#if NMS > 0
			extern int _ms_helper();

			if (xputc(code, SCC_MOUSE) < 0)
				printf("mouse queue overflow\n");
			/* KU:XXX softcall? */
			timeout(_ms_helper, (caddr_t)0, 0);
#endif
			break;
		    }
		    case SCC_KEYBOARD: {
#if NBM > 0
			extern int kb_softint();

			if (xputc(code, SCC_KEYBOARD) < 0)
				printf("keyboard queue overflow\n");
			/* KU:XXX softcall? */
			timeout(kb_softint, (caddr_t)0, 0);
#endif
			break;
		    }
		    default:
			printf("kb or ms stray intr\n");
			break;
		}
	}

#ifdef news3400
	*inte |= Kbm_port[chan].intr_en;
#else
	*inte = 1;
#endif
}


kbm_write(chan, buf, count)
	int chan;
	char *buf;
	register int count;
{
	register u_char *port = Kbm_port[chan].buzz_port;
	int c_save = count;

#ifdef news3400
	*port = count / 3;
#endif

	return (c_save);
}

int
kbm_getc(chan)
	int chan;
{
#ifdef news3400
	volatile u_char *port = (volatile u_char *)Kbm_port[chan].data_port;
	volatile u_char *stat = (volatile u_char *)Kbm_port[chan].stat_port;
#else
	register u_char *port = Kbm_port[chan].data_port;
	register u_char *stat = Kbm_port[chan].stat_port;
#endif
	int rdy = Kbm_port[chan].data_rdy;

	if (*stat & rdy) 
		return (*port & 0xff);
	else
		return (-1);
}
