/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
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
 * from: $Hdr: scc.c,v 4.300 91/06/09 06:44:53 root Rel41 $ SONY
 *
 *	@(#)scc.c	8.1 (Berkeley) 6/11/93
 */

/*
 * LH8530 SCC (serial communication controller) driver
 *
 * NOTE: This driver is available only for news700/1200/1700/3400.
 *
 * Any code and mechanism in this module may not be used
 * in any form without permissions.  COPYRIGHT (C) 1989-
 * SONY  Corporation,   Super Microsystems Group (SMSG),
 * Work Station Division, all rights RESERVED.
 */

#include <machine/adrsmap.h>

#include "rs.h"

#include <sys/param.h>
#include <sys/ioctl.h>
#include <sys/tty.h>
#include <sys/malloc.h>

#include <news3400/sio/sccparam.h>
#include <news3400/sio/sccreg.h>
#include <news3400/sio/scc.h>
#include <news3400/sio/scc.conf>

#define	PROBE_DATA	0x55

#ifdef mips
#define	VOLATILE	volatile
#else
#define	VOLATILE
#endif

int	tty00_is_console = 0;

#define	SCC_BUFSIZE	256

char	scc_buf[2][SCC_BUFSIZE];

scc_open(chan)
	int chan;
{
	register Scc_channel *scc = &sccsw[chan];
	register int s;

	s = splscc();
	if ((scc->scc_status & CHAN_ACTIVE) == 0) {
		scc_init(chan);
		if (chan <= SCC_REMOTE1)
			scc->r_dma.dma_addr = scc_buf[chan];
		else
			scc->r_dma.dma_addr =
				malloc(SCC_BUFSIZE, M_DEVBUF, M_WAITOK);
		scc->r_dma.dma_count = 0;
		scc->scc_status |= CHAN_ACTIVE;
	}
	(void) splx(s);
	return (0);
}

scc_probe(chan)
	register int chan;
{
	VOLATILE struct scc_reg *port = sccsw[chan].scc_port;
	int s, temp, probe;

	if (badaddr(port, 1))
		return (0);
	s = splscc();
	temp = scc_read_reg(chan, RR12);
	scc_write_reg(chan, WR12, PROBE_DATA);
	probe = scc_read_reg(chan, RR12);
	scc_write_reg(chan, WR12, temp);
	(void) splx(s);
	return (probe == PROBE_DATA);
}

scc_getc(chan)
	int chan;
{
	VOLATILE struct scc_reg *port = sccsw[chan].scc_port;
	int c;

	if (port->ctrl & R0_RxCA) {
		SCCWAIT;
		c = port->data;
		SCCWAIT;
		return (c);
	}
	SCCWAIT;
	return (-1);
}

#ifndef notdef
scc_putc(chan, c)
	int chan, c;
{
	register VOLATILE struct scc_reg *port = sccsw[chan].scc_port;

	while ((port->ctrl & R0_TxBE) == 0)
		SCCWAIT;
	SCCWAIT;
	port->data = c;
	SCCWAIT;
}
#else
scc_putc(chan, c)
        int chan, c;
{
	register VOLATILE struct scc_reg *port = sccsw[chan].scc_port;
	register VOLATILE u_char *ctrl = &sccsw[chan].scc_port->ctrl;
	register VOLATILE u_char *data = &sccsw[chan].scc_port->data;

        SCCWAIT;
        while ((*ctrl & R0_TxBE) == 0) {
                SCCWAIT;
        }
        SCCWAIT;

        *ctrl = W0_RES_TxINT;
        SCCWAIT;
        *data = (char)(c & 0xff);
        SCCWAIT;
        scc_write_reg(chan, WR1, W1_RxINT_ALL|W1_TxINTE|W1_EXTINTE|W1_PARITY);
        SCCWAIT;
}
#endif

scc_init(chan)
	int chan;
{
	register VOLATILE struct scc_reg *port;
	register char *data;
	register int i;
	register Scc_channel *scc = &sccsw[chan];
	int s;

	s = splscc();
	data = scc->scc_init;
	port = scc->scc_port;
	for (i = 0; i < N_INITDATA; i++) {
		port->ctrl = *data++;
		SCCWAIT;
	}
	scc_write_reg(chan, WR2, scc->scc_vec & ~0x0f);
/* KU:XXX
This must be bug because scc->scc_param is not initialized yet.
	scc_set_param(chan, scc->scc_param);
*/
	(void) splx(s);
}

#define	vec_to_scc(vec)		((((vec) - SCCVEC0) >> 3) & 0x000f)
#define	vec_to_chan(vec)	scc2chan[vec_to_scc(vec)]

int scc2chan[] = {
	1,	0,
	3,	2,
	5,	4,
	7,	6,
	9,	8
};

scc_rint(vec)
	int vec;
{
	int chan = vec_to_chan(vec);
	register Scc_channel *scc = &sccsw[chan];
	register VOLATILE struct scc_reg *port = scc->scc_port;
	register int c;

	if ((scc->scc_status & CHAN_ACTIVE) == 0) {
		scc_reset(chan);
		goto out;
	}
	if (scc->scc_status & LINE_BREAK){
		scc->scc_status &= ~LINE_BREAK;
		c = port->data;
		SCCWAIT;
	}
	while (port->ctrl & R0_RxCA) {
		SCCWAIT;
		c = port->data;
		SCCWAIT;
#if NRS > 0
		scc_pdma(chan, c);
#endif
	}
	SCCWAIT;
out:
	port->ctrl = W0_RES_IUS;
	SCCWAIT;
}

#if NRS > 0
scc_enable(chan)
	int chan;
{
	register Scc_channel *scc = &sccsw[chan];
	int n;
	int s;

	s = splscc();
	if ((n = scc->r_dma.dma_count) > 0) {
		scc->r_dma.dma_count = 0;
		rsrint(chan, scc->r_dma.dma_addr, n);
	} else
		scc->scc_status |= ENABLE;
	(void) splx(s);
}

scc_pdma(chan, c)
	int chan;
	int c;
{
	register Scc_channel *scc = &sccsw[chan];
	int n;

	if (scc->r_dma.dma_count >= SCC_BUFSIZE)
		printf("rs%d soft fifo overflow\n", chan);
	else
		scc->r_dma.dma_addr[scc->r_dma.dma_count++] = c;
	if (scc->scc_status & ENABLE || scc->r_dma.dma_count >= SCC_BUFSIZE) {
		scc->scc_status &= ~ENABLE;
		n = scc->r_dma.dma_count;
		scc->r_dma.dma_count = 0;
		rsrint(chan, scc->r_dma.dma_addr, n);
	}
}
#endif /* NRS > 0 */

scc_xint(vec)
	int vec;
{
	int chan = vec_to_chan(vec);
	register Scc_channel *scc = &sccsw[chan];
	register VOLATILE struct scc_reg *port = scc->scc_port;

	if (scc->scc_status & OSTOP)
		scc->scc_status &= ~(OACTIVE|OSTOP);
	if (scc->scc_status & OFLUSH) {
		scc->x_dma.dma_count = 0;
		scc->scc_status &= ~(OACTIVE|OFLUSH);
	}
	if ((scc->scc_status & OACTIVE) && (scc->x_dma.dma_count > 0)) {
		port->data = *(scc->x_dma.dma_addr)++;
		SCCWAIT;
		scc->x_dma.dma_count--;
	} else {
		port->ctrl = W0_RES_TxINT;
		SCCWAIT;
		scc->scc_status &= ~OACTIVE;
#if NRS > 0
		if (scc->x_dma.dma_count == 0)
			rsxint(chan);
#endif
	}
	port->ctrl = W0_RES_IUS;
	SCCWAIT;
}

scc_sint(vec)
	int vec;
{
	int chan = vec_to_chan(vec);
	register Scc_channel *scc = &sccsw[chan];
	register VOLATILE struct scc_reg *port = scc->scc_port;
	register int status;
	register int param = 0;

	port->ctrl = W0_RES_EXT;
	SCCWAIT;
	if ((scc->scc_status & CHAN_ACTIVE) == 0) {
		scc_reset(chan);
		goto out;
	}
	status = port->ctrl;
	SCCWAIT;
	if (status & R0_DCD)
		param |= DCD;
	if (status & R0_CTS)
		param |= CTS;
	if (status & R0_BREAK){
		param |= RBREAK;
		scc->scc_status |= LINE_BREAK;
	}
	if ((scc->scc_param & (DCD|CTS|RBREAK)) != param) {
		scc->scc_param = (scc->scc_param & ~(DCD|CTS|RBREAK)) | param;
#if NRS > 0
		rssint(chan, scc->scc_param);
#endif
	}
out:
	port->ctrl = W0_RES_IUS;
	SCCWAIT;
}

scc_cint(vec)
	int vec;
{
	int chan = vec_to_chan(vec);
	register Scc_channel *scc = &sccsw[chan];
	register VOLATILE struct scc_reg *port = scc->scc_port;
	register int status;
	int c;

	if ((scc->scc_status & CHAN_ACTIVE) == 0) {
		scc_reset(chan);
		goto out;
	}
	status = scc_read_reg(chan, RR1);
	if (status & R1_CRC)
		scc->scc_param |= FRAMING_ERROR;
	if (status & R1_OVRUN) {
		if ((scc->scc_param & OVERRUN_ERROR) == 0) {
			scc->scc_param |= OVERRUN_ERROR;
#if NRS > 0
			rssint(chan, scc->scc_param);
#endif
		}
	}
	if (status & R1_PARITY) {
		scc->scc_param |= SCC_PARITY_ERROR;
		while (port->ctrl & R0_RxCA) {
			SCCWAIT;
			c = port->data;
			SCCWAIT;
#if NRS > 0
			if (scc->scc_param & NOCHECK)
				scc_pdma(chan, c);
#endif
		}
		SCCWAIT;
	}
out:
	port->ctrl = W0_RES_ERROR;
	SCCWAIT;
	port->ctrl = W0_RES_IUS;
	SCCWAIT;
}

scc_write_reg(chan, reg, data)
	int chan, reg, data;
{
	register VOLATILE struct scc_reg *port = sccsw[chan].scc_port;

	port->ctrl = reg;
	SCCWAIT;
	port->ctrl = data;
	SCCWAIT;
}

scc_read_reg(chan, reg)
	int chan, reg;
{
	register VOLATILE struct scc_reg *port = sccsw[chan].scc_port;
	int result;

	port->ctrl = reg;
	SCCWAIT;
	result = port->ctrl;
	SCCWAIT;
	return (result);
}

#ifdef news3400
#define	DSRA	0x01
#define	RIA	0x02
#define	DSRB	0x04
#define	RIB	0x08

#define	DSRC	0x01
#define	RIC	0x02
#define	DSRD	0x04
#define	RID	0x08
#define	DSRE	0x10
#define	RIE	0x20
#define	DSRF	0x40
#define	RIF	0x80
#endif /* news3400 */

struct ri_dsr {
	char	*status;
	int	ri;
	int	dsr;
} ri_dsr[] = {
	{ (char *)SCC_STATUS0, RIA, DSRA },
	{ (char *)SCC_STATUS0, RIB, DSRB },
#if !defined(news3200)
	{ (char *)SCC_STATUS1, RIC, DSRC },
	{ (char *)SCC_STATUS1, RID, DSRD },
	{ (char *)SCC_STATUS1, RIE, DSRE },
	{ (char *)SCC_STATUS1, RIF, DSRF },
	{ (char *)SCC_STATUS2, RIC, DSRC },
	{ (char *)SCC_STATUS2, RID, DSRD },
	{ (char *)SCC_STATUS2, RIE, DSRE },
	{ (char *)SCC_STATUS2, RIF, DSRF }
#endif /* !news3200 */
};

get_ri_dsr(chan)
	int chan;
{
	register struct ri_dsr *p;
	register int status, param;

	param = 0;
	p = &ri_dsr[chan];
	status = *p->status;
	if ((status & p->ri) == 0)
		param |= RI;
	if ((status & p->dsr) == 0)
		param |= DSR;
	return (param);
}

#if defined(news3400)
/*
 *	tc0 = floor(4915200 / 32 / baudrate - 2 + 0.5);
 */
static int tc0[] = {
	0,		/* B0 */
	3070,		/* B50 */
	2046,		/* B75 */
	1394,		/* B110 */
	1144,		/* B134 */
	1022,		/* B150 */
	766,		/* B200 */
	510,		/* B300 */
	254,		/* B600 */
	126,		/* B1200 */
	83,		/* B1800 */
	62,		/* B2400 */
	30,		/* B4800 */
	14,		/* B9600 */
	6,		/* EXTA (B19200) */
	2		/* EXTB (B38400) */
	};
#endif /* news3400 */

static int tc1[] = {
/*
 *	tc1 = floor(3686400 / 32 / baudrate - 2 + 0.5);
 */
	0,		/* B0 */
	2302,		/* B50 */
	1534,		/* B75 */
	1045,		/* B110 */
	858,		/* B134 */
	766,		/* B150 */
	574,		/* B200 */
	382,		/* B300 */
	190,		/* B600 */
	94,		/* B1200 */
	62,		/* B1800 */
	46,		/* B2400 */
	22,		/* B4800 */
	10,		/* B9600 */
	4,		/* B19200 */
	1,		/* B38400 */
};

scc_set_param(chan, param)
	int chan;
	register int param;
{
	register Scc_channel *scc = &sccsw[chan];
	register int bit, baud, *tc;
	int s;

	s = splscc();

	/*
	 * Baud rate / external clock
	 */
	if ((baud = param & BAUD_RATE) == EXTB && chan <= SCC_REMOTE1 &&
	    param & EXTCLK_ENABLE) {
		scc_write_reg(chan, WR11, W11_RxC_RTxC|W11_TxC_TRxC);
		bit = W4_X1;
	} else {
		tc = (chan <= SCC_REMOTE1) ? tc0 : tc1;
		scc_write_reg(chan, WR11, W11_RxC_BRG|W11_TxC_BRG);
		scc_write_reg(chan, WR12, tc[baud] & 0xff);
		scc_write_reg(chan, WR13, tc[baud] >> 8);
		bit = W4_X16;
	}

	/*
	 * Clock mode / parity / stop bit
	 */
	if (param & PARITY) {
		bit |= W4_PARITY;
		if (param & EVEN)
			bit |= W4_EVEN;
	}
	switch (param & STOPBIT) {

	case STOP1:
		bit |= W4_STOP1;
		break;

	case STOP1_5:
		bit |= W4_STOP1_5;
		break;

	case STOP2:
		bit |= W4_STOP2;
		break;

	}
	scc_write_reg(chan, WR4, bit);

	/*
	 * Receiver enable / receive character size / auto enable
	 */
	bit = (param & RXE ? W3_RxE : 0);
	switch (param & CHAR_SIZE) {

	case C5BIT:
		break;

	case C6BIT:
		bit |= W3_Rx6BIT;
		break;

	case C7BIT:
		bit |= W3_Rx7BIT;
		break;

	case C8BIT:
		bit |= W3_Rx8BIT;
		break;
	}
#ifdef AUTO_ENABLE
	if (param & AUTO_ENABLE)
		bit |= W3_AUTO;
#endif /* AUTO_ENABLE */
	scc_write_reg(chan, WR3, bit);

	/*
	 * Transmitter enable / transmit character size / RTS / DTR / BREAK
	 */
	bit = (param & TXE ? W5_TxE : 0);
	switch (param & CHAR_SIZE) {

	case C5BIT:
		break;

	case C6BIT:
		bit |= W5_Tx6BIT;
		break;

	case C7BIT:
		bit |= W5_Tx7BIT;
		break;

	case C8BIT:
		bit |= W5_Tx8BIT;
		break;
	}
	if (param & RTS)
		bit |= W5_RTS;
	if (param & DTR)
		bit |= W5_DTR;
	if (param & XBREAK)
		bit |= W5_BREAK;
	scc_write_reg(chan, WR5, bit);
	scc->scc_param = param;
	(void) splx(s);
	return (0);
}

scc_get_param(chan)
	int chan;
{
	register Scc_channel *scc = &sccsw[chan];

	scc->scc_param = (scc->scc_param & ~(RI|DSR)) | get_ri_dsr(chan);
	return (scc->scc_param);
}

scc_get_status(chan)
	int chan;
{

	return (sccsw[chan].scc_status);
}

scc_set_status(chan, stat)
	int chan, stat;
{

	sccsw[chan].scc_status = stat;

	return (0);
}

scc_flush(chan)
	int chan;
{
	register Scc_channel *scc = &sccsw[chan];

	if (scc->scc_status & OACTIVE)
		scc->scc_status |= OFLUSH;
	else if (scc->x_dma.dma_count > 0) {
		scc->x_dma.dma_count = 0;
#if NRS > 0
		rsxint(chan);
#endif
	}
	return (0);
}

scc_start(chan)
	int chan;
{
	register Scc_channel *scc = &sccsw[chan];

	if ((scc->scc_status & OACTIVE) == 0 && scc->x_dma.dma_count > 0) {
		scc->scc_port->data = *(scc->x_dma.dma_addr)++;
		SCCWAIT;
		scc->x_dma.dma_count--;
		scc->scc_status |= OACTIVE;
	}
	return (0);
}

scc_stop(chan)
	int chan;
{
	register Scc_channel *scc = &sccsw[chan];

	if (scc->scc_status & OACTIVE)
		scc->scc_status |= OSTOP;
	return (0);
}

scc_write(chan, buf, count)
	int chan;
	caddr_t buf;
	int count;
{
	register Scc_channel *scc = &sccsw[chan];

	if (count <= 0)
		return (0);
	scc->x_dma.dma_addr = buf;
	scc->x_dma.dma_count = count;
	scc_start(chan);
	return (count);
}

scc_error_write(chan, buf, count)
	int chan;
	register char *buf;
	register int count;
{
	register int i;

	for (i = 0; i < count; i++)
		scc_putc(chan, *buf++);
	return (i);
}

scc_reset(chan)
	int chan;
{
	register Scc_channel *scc = &sccsw[chan];

	while (scc_getc(chan) != -1)
		;
	scc->scc_status &= ~CHAN_ACTIVE;
}
