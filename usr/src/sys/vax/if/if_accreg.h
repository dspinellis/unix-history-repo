/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.
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
 *	@(#)if_accreg.h	7.3 (Berkeley) 6/28/90
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
