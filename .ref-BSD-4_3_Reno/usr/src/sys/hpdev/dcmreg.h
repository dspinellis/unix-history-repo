/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1982, 1986, 1990 The Regents of the University of California.
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
 * from: Utah $Hdr: dcmreg.h 1.3 89/08/23$
 *
 *	@(#)dcmreg.h	7.3 (Berkeley) 6/22/90
 */

struct dcmdevice {	   /* host address, only odd bytes addressed */
	u_char	dcm_pad0;
	volatile u_char	dcm_rsid;	/* Reset / ID			0001 */
	u_char	dcm_pad1;
	volatile u_char	dcm_ic;		/* Interrupt control register	0003 */
	u_char	dcm_pad2;
	volatile u_char	dcm_sem;	/* Semaphore register		0005 */
	u_char  dcm_pad3[0x7ffa];	/* Unaddressable	0006-7fff */
	u_char	dcm_pad4;
	volatile u_char	dcm_iir;	/* Interrupt ident register	8001 */
	u_char	dcm_pad5;
	volatile u_char	dcm_cr;		/* Command register		8003 */
	u_char  dcm_pad6[0x3fc];	/* Card scratch		8004-83ff */
	struct	dcmrfifo {
		u_char	ptr_pad1;
		volatile u_char	data_char;
		u_char	ptr_pad2;
		volatile u_char	data_stat;
	} dcm_rfifos[4][0x80];		/* Receive queues		8400 */
	struct  {
		u_char	ptr_pad1;
		volatile u_char	data_data;
	} dcm_bmap[0x100];		/* Bitmap table			8c00 */
	struct  {
		u_char	ptr_pad;
		volatile u_char	ptr;
	} dcm_rhead[4];			/* Fifo head - receive		8e00 */
	struct  {
		u_char  ptr_pad;
		volatile u_char  ptr;
	} dcm_rtail[4];			/* Fifo tail - receive		8e08 */
	struct  {
		u_char	ptr_pad;
		volatile u_char	ptr;
	} dcm_thead[4];			/* Fifo head - transmit		8e10 */
	struct  {
		u_char	ptr_pad;
		volatile u_char	ptr;
	} dcm_ttail[4];			/* Fifo tail - transmit		8e18 */
	struct  {
		u_char	pad1;
		volatile u_char	dcm_conf;
		u_char	pad2;
		volatile u_char	dcm_baud;
	} dcm_data[4];			/* Configuration registers	8e20 */
	u_char  dcm_pad7;
	volatile u_char  dcm_mdmin;	/* Modem in			8e31 */
	u_char  dcm_pad8;
	volatile u_char  dcm_mdmout;	/* Modem out			8e33 */
	u_char  dcm_pad9;
	volatile u_char  dcm_mdmmsk;	/* Modem mask			8e35 */
	struct  {
		u_char pad1;
		volatile u_char dcm_data;
	} dcm_cmdtab[4];		/* Command tables		8e36 */
	struct  {
		u_char pad1;
		volatile u_char dcm_data;
	} dcm_icrtab[4];		/* Interrupt data		8e3e */
	u_char  dcm_pad10;
	volatile u_char  dcm_stcon;	/* Self test condition		8e47 */
	u_char  dcm_pad11[0x98];	/* Undef SR regs	8e48-8edf */
	struct	dcmtfifo {
	    u_char  ptr_pad1;
	    volatile u_char  data_char;
	} dcm_tfifos[4][0x10];		/* Transmit queues		8ee0 */
};

/*
 * Overlay structure for port specific queue "registers".
 * Starts at offset 0x8E00+(port*2).
 */
struct	dcmpreg {
	u_char		pad0;		/* +00 */
	volatile u_char	r_head;		/* +01 */
	u_char		pad1[7];	/* +02 */
	volatile u_char	r_tail;		/* +09 */
	u_char		pad2[7];	/* +0A */
	volatile u_char	t_head;		/* +11 */
	u_char		pad3[7];	/* +12 */
	volatile u_char	t_tail;		/* +19 */
};
#define	dcm_preg(d, p)	((struct dcmpreg *)((u_int)(d)+0x8e00+(p)*2))

/* interface reset/id */
#define DCMCON          0x80	/* REMOTE/LOCAL switch, read */
#define	DCMID		0x5	/* hardwired card id, read */
#define	DCMRS		0x80	/* software reset, write */

/* interrupt control */
#define	DCMIPL(x)	((((x) >> 4) & 3) + 3)	/* interupt level, read */
#define	IC_IR		0x40	/* interupt request, read */
#define	IC_IE		0x80	/* interupt enable, write */
#define	IC_ID		0x00	/* interupt disable, write */


/* Semaphore control */
#define	SEM_BSY		0x80	/* read */
#define SEM_CLR         0xFF	/* write */
#define SEM_LOCK(dcm)	while ((dcm)->dcm_sem & SEM_BSY)
#define SEM_UNLOCK(dcm)	(dcm)->dcm_sem = SEM_CLR

/* command register */
#define	CR_PORT0	0x1
#define	CR_PORT1	0x2
#define	CR_PORT2	0x4
#define	CR_PORT3	0x8
#define	CR_MODM		0x10	/* change modem output lines */
#define	CR_TIMER	0x20	/* 16ms interrupt timer toggle */
#define	CR_SELFT	0x40	/* run self test */
#define CR_MASK		0x7f

/* interrupt ident register */
#define	IIR_PORT0	0x1
#define	IIR_PORT1	0x2
#define	IIR_PORT2	0x4
#define	IIR_PORT3	0x8
#define	IIR_SELFT	0x10	/* self test completed */
#define	IIR_MODM	0x20	/* change in modem input lines */
#define	IIR_TIMEO	0x40	/* Time out */
#define IIR_MASK	0x7f

/* self test cond reg */
#define ST_OK           0xe0

/* Line configuration register */
#define	LC_PNO		0x00
#define	LC_PODD		0x01
#define	LC_PEVEN	0x02
#define	LC_PMSK		0x03

#define	LC_1STOP	0x00
#define	LC_1HSTOP	0x04
#define	LC_2STOP	0x08
#define	LC_STOPMSK	0x0b

#define	LC_8BITS	0x30
#define	LC_7BITS	0x20
#define	LC_6BITS	0x10
#define	LC_5BITS	0x00
#define	LC_BITMSK	0x30

/* baud reg */
#define BR_0		0x00
#define BR_50		0x01
#define BR_75		0x02
#define BR_110		0x03
#define BR_134  	0x04
#define BR_150		0x05
#define BR_300		0x06
#define BR_600		0x07
#define BR_900		0x08
#define BR_1200		0x09
#define BR_1800		0x0a
#define BR_2400		0x0b
#define BR_3600		0x0c
#define BR_4800		0x0d
#define BR_7200		0x0e
#define BR_9600		0x0f
#define BR_19200	0x10
#define BR_38400	0x11

/* modem input register */
#define	MI_CTS		0x08
#define	MI_DM		0x04
#define	MI_CD		0x02
#define	MI_RI		0x01

/* modem output register */
#define	MO_SR		0x04
#define	MO_DTR		0x02
#define	MO_RTS		0x01
#define	MO_ON		((MO_DTR) | (MO_RTS))
#define	MO_OFF		0x00

/* cmd-tab values, write */
#define CT_CON		0x1	/* configuration change */
#define CT_TX		0x2	/* transmit buffer not empty */
#define CT_BRK		0x4	/* toggle BREAK */

/* icr-tab values, read */
#define IT_TX		0x1	/* transmit buffer empty */
#define IT_SPEC		0x2	/* special character received */

/* data errors */
#define RD_OVF		0x08
#define RD_BD		0x10
#define RD_PE		0x20
#define RD_OE		0x40
#define RD_FE		0x80
#define RD_MASK		0xf8

/* Transmit/Receive masks */
#define TX_MASK		0x0f
#define RX_MASK		0xff

/*
 * WARNING: Serial console is assumed to be the lowest select-code card
 * and that card must be logical unit 0 in the kernel.  Also, CONUNIT must
 * be 1, the port affected by the REMOTE/LOCAL switch.
 */
#define CONUNIT	(1)
