/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)if_accreg.h	7.2 (Berkeley) %G%
 */

/*
 * ACC LH/DH-11 interface
 */

struct accdma {
	short	csr;	/* control and status */
	short	db;	/* data buffer */
	u_short	ba;	/* buss address */
	short	wc;	/* word count */
};

struct accdevice {
	struct	accdma input;
	struct	accdma output;
};

#define	icsr	input.csr
#define	iba	input.ba
#define	iwc	input.wc
#define	ocsr	output.csr
#define	oba	output.ba
#define	owc	output.wc

/*
 * Bits Common to both input and out CSR's
 */
#define	ACC_ERR		0x8000		/* error present */
#define	ACC_NXM		0x4000		/* non-existant memory */
#define	ACC_RDY		0x0080		/* ready */
#define	ACC_IE		0x0040		/* interrupt enable */
#define	ACC_RESET	0x0002		/* reset interface */
#define	ACC_GO		0x0001		/* start operation */

/*
 * Input Control Status Register
 */
#define IN_EOM		0x2000		/* end-of-message recieved */
#define IN_HRDY		0x0800		/* host ready */
#define IN_IMPBSY	0x0400		/* IMP not ready */
#define IN_RMR		0x0200		/* receive master ready error */
#define IN_IBF		0x0100		/* input data buffer full */
#define IN_WEN		0x0008		/* write enable */
#define IN_MRDY		0x0004		/* master ready */

#define ACC_INBITS \
"\20\20ERR\17NXM\16EOM\14HRDY\13IMPBSY\12RMR\11IBF\10RDY\7IE\
\4WEN\3MRDY\2RESET\1GO"

/*
 * Output Control Status Register
 */
#define OUT_TMR		0x0200		/* transmit master ready error */
#define OUT_BBACK	0x0008		/* bus back */
#define OUT_ENLB 	0x0004		/* enable last bit */

#define ACC_OUTBITS \
"\20\20ERR\17NXM\12TMR\10RDY\7IE\4BBACK\3ENLB\2RESET\1GO"
