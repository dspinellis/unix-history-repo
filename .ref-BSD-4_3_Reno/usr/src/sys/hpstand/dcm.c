/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)dcm.c	7.1 (Berkeley) 5/25/90
 */

#ifdef DCMCONSOLE
#include "param.h"
#include "machine/cons.h"
#include "../hpdev/device.h"
#include "../hpdev/dcmreg.h"

#define CONSPORT	(1)
struct dcmdevice *CONSOLE = NULL;

dcmprobe(cp)
	struct consdev *cp;
{
	extern struct hp_hw sc_table[];
	register struct hp_hw *hw;
	register struct dcmdevice *dcm;

	for (hw = sc_table; hw < &sc_table[MAX_CTLR]; hw++)
	        if (hw->hw_type == COMMDCM && !badaddr((caddr_t)hw->hw_addr))
			break;
	if (hw->hw_type != COMMDCM) {
		cp->cn_pri = CN_DEAD;
		return;
	}
	CONSOLE = (struct dcmdevice *)hw->hw_addr;

	dcm = CONSOLE;
	switch (dcm->dcm_rsid) {
	case DCMID:
		cp->cn_pri = CN_NORMAL;
		break;
	case DCMID|DCMCON:
		cp->cn_pri = CN_REMOTE;
		break;
	default:
		cp->cn_pri = CN_DEAD;
		break;
	}
}

dcminit(cp)
	struct consdev *cp;
{
	register struct dcmdevice *dcm = CONSOLE;
	register int port = CONSPORT;

	dcm->dcm_ic = IC_ID;
	while (dcm->dcm_thead[port].ptr != dcm->dcm_ttail[port].ptr)
		;
	dcm->dcm_data[port].dcm_baud = BR_9600;
	dcm->dcm_data[port].dcm_conf = LC_8BITS | LC_1STOP;
	SEM_LOCK(dcm);
	dcm->dcm_cmdtab[port].dcm_data |= CT_CON;
	dcm->dcm_cr |= (1 << port);
	SEM_UNLOCK(dcm);
	DELAY(15000);
}

#ifndef SMALL
dcmgetchar()
{
	register struct dcmdevice *dcm = CONSOLE;
	register struct dcmrfifo *fifo;
	register struct dcmpreg *pp;
	register unsigned head;
	int c, stat, port;

	port = CONSPORT;
	pp = dcm_preg(dcm, port);
	head = pp->r_head & RX_MASK;
	if (head == (pp->r_tail & RX_MASK))
		return(0);
	fifo = &dcm->dcm_rfifos[3-port][head>>1];
	c = fifo->data_char;
	stat = fifo->data_stat;
	pp->r_head = (head + 2) & RX_MASK;
	SEM_LOCK(dcm);
	stat = dcm->dcm_iir;
	SEM_UNLOCK(dcm);
	return(c);
}
#else
dcmgetchar()
{
	return(0);
}
#endif

dcmputchar(c)
	register int c;
{
	register struct dcmdevice *dcm = CONSOLE;
	register struct dcmpreg *pp;
	register int timo;
	unsigned tail;
	int port, stat;

	port = CONSPORT;
	pp = dcm_preg(dcm, port);
	tail = pp->t_tail & TX_MASK;
	timo = 50000;
	while (tail != (pp->t_head & TX_MASK) && --timo)
		;
	dcm->dcm_tfifos[3-port][tail].data_char = c;
	pp->t_tail = tail = (tail + 1) & TX_MASK;
	SEM_LOCK(dcm);
	dcm->dcm_cmdtab[port].dcm_data |= CT_TX;
	dcm->dcm_cr |= (1 << port);
	SEM_UNLOCK(dcm);
	timo = 1000000;
	while (tail != (pp->t_head & TX_MASK) && --timo)
		;
	SEM_LOCK(dcm);
	stat = dcm->dcm_iir;
	SEM_UNLOCK(dcm);
}
#endif
