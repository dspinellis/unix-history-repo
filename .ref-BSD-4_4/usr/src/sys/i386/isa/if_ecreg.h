/*
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
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
 *	@(#)if_ecreg.h	8.1 (Berkeley) 6/11/93
 */
/*
 * Device definitions for the i82586 generally and the 3com ec507
 * ISA bus ethernet controller.
 */
/*
 * Intermediate System Control Block pointer
 */
struct	ec_iscp {
	u_short	busy;
	u_short	scb_off;
	caddr_t	scb_base;
};
/*
 * System Control Block
 */
struct	ec_scb {
	u_short status;
#define CX		0x8000
#define FR		0x4000
#define CNA		0x2000
#define RNR		0x1000
#define CU_STATE	0x700
#define	CUS_IDLE	0x000
#define	CUS_SUSPENDED	0x100
#define	CUS_ACTIVE	0x200
#define RU_STATE	0x70
#define	RUS_IDLE	0x00
#define	RUS_SUSPENDED	0x10
#define	RUS_OVERRUN	0x20
#define	RUS_READY	0x40
	u_short command;
#define	CU_NOP		0
#define CU_START	0x100
#define RU_NOP		0
#define	RU_START	0x100
#define	RU_ABORT	0x400
#define	ACK_CX		0x8000
#define	ACK_RX		0x4000
#define ACK_CNA		0x2000
#define ACK_RNR		0x1000
	u_short	cbl_off;
	u_short	rfa_off;
	struct	errors {
		u_short crc;
		u_short alignment;
		u_short resource;
		u_short overrun;
	} scb_errs;
};

/*
 * Individual Address Setup
 */
struct ec_iasetup {
	u_short	com0;
#define	COM0_A	0x1000
#define	COM0_OK	0x2000
#define	COM0_B	0x4000
#define	COM0_C	0x8000
	u_short	com1;
#define	COM1_IASETUP	0x1
#define	COM1_I	0x2000
#define	COM1_S	0x4000
#define	COM1_EL	0x8000
	u_short	next_off;
	u_char	srcaddr[6];
};
	
/*
 * Load Multicast Addresses
 */

struct ec_mcsetup {
	u_short	com0;
	u_short	com1;
#define COM1_MCSETUP	0x3
	u_short	next_off;
	u_short	count;		/* number of bytes, 14 bits only */
	/* char	mcaddrs[6];	/* white lie, data goes here */
};

/*
 * Transmit Command
 */
struct	ec_transmit {
	u_short com0;
#define COLLISIONS(p) (p->com0 & 0xf)
#define EXCOL		0x10	/* Excessive Collisions, Aborted */
#define HEARTBEAT	0x20	/* CDT signal monitored and OK (good) */
#define TXDEFFERED	0x40	/* ``due to Previous Link Activity'' */
#define	DMALATE		0x80	/* DMA underrun */
#define	NOCTS		0x100	/* Transmission stopped due to ... */
#define	NOCARSENSE	0x200	/* Loss of Carrier sense during TX */
#define	LATECOLL	0x400	/* Late Collision detected */
#define TXERRS		0x7d0	/* Various Bad Things */
	u_short	com1;
#define COM1_TRANSMIT	0x4
	u_short	next_off;
	u_short tbd_off;
	u_char	dstaddr[6];
	u_short	proto;		/* Length for 802.3 */
/*
 * };	 struct	ec_tba {
 *
 * Transmit Buffer Descriptor
 * This really should be a separate structure,
 * but it will be convenient to lump them together.
 */
	u_short	count;
	u_short next_tbd_off;
	caddr_t	buffer;
};

/*
 * Receive Frame Descriptor
 */
struct	ec_rfd {
	u_short	rfd0;
	u_short rfd1;
	u_short	next_off;
	u_short rbd_off;
	struct	ether_header eh;
	u_short	mbz;
/*
 * };	struct	ec_rbd {
 *
 * Receive Buffer Descriptor
 */
	u_short	count;
#define RBD_EOF	0x8000
#define RBD_F	0x4000
	u_short	next_rbd_off;
	caddr_t	buffer;
	u_short	size;
	u_short	mbz2;
};

/*
 * Set Operating Parameters
 */
struct	ec_82586params {
	u_char	count;
	u_char	fifolimit;
	u_char	save_bad_frames;
	u_char	data1;		/* addr len, no src ins, lpbk, prmble */
	u_char	priority;
	u_char	ifspacing;
	u_char	mbz1;
	u_char	data2;		/* max restries, slot time high */
	u_char	promisc;	/* brddis, mnch/nrz, tnocr, crc16, bstf, pad */
#define M_PROMISC 1
#define M_TNOCR 8
	u_char	cdf_cds;	/* carrier detect/sense filter(length) & src */
	u_char	min_frame_len;
	u_char	mbz2[3];
};

struct	ec_configure {
	u_short	com0;
	u_short com1;
#define COM1_CONFIGURE	0x2
	u_short next_off;
	struct ec_82586params modes;
};
#define ECMTU	1518
#define ECMINSIZE 64
#define NTXBUF	2
#define	NRXBUF	8

struct	ec_mem {
	struct	ec_iscp iscp;
	struct	ec_scb scb;
	struct	ec_transmit tcom[NTXBUF];
	struct	ec_rfd rcom[NRXBUF];
	struct	ec_configure config;
	struct	ec_iasetup iasetup;
	struct	ec_mcsetup mcsetup;
	char	txbuf[NTXBUF][ECMTU];
	char	rxbuf[NRXBUF][ECMTU];
};

struct ec_ports {
	u_char	data[6];
	u_char	creg;
#define R_ECID	0x00
#define R_ETHER	0x01
#define R_REV	0x02
#define R_IEN	0x04
#define R_INT	0x08
#define R_LAD	0x10
#define R_LPB	0x20
#define R_CA	0x40
#define R_NORST	0x80
	u_char	mbz0[3];
	u_char	port_ic;
	u_char	port_ca;
	u_char	mbz1;
	u_char	rom_conf;
	u_char	ram_conf;
	u_char	int_conf;
};
