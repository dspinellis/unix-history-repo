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
 * from: $Hdr: sccreg.h,v 4.300 91/06/09 06:44:58 root Rel41 $ SONY
 *
 *	@(#)sccreg.h	8.1 (Berkeley) 6/11/93
 */

#ifndef _SCCREG_
#define _SCCREG_

/*
 *	SCC register
 */

struct scc_reg {
	u_char	ctrl;			/* control register	*/
	u_char	data;			/* data    register	*/
};

/*
 *	SCC read register
 */

#define	RR0		0
#define	RR1		1
#define	RR2		2
#define	RR3		3
#define	RR10		10
#define	RR12		12
#define	RR13		13
#define	RR15		15

#define	R0_BREAK	0x80		/* Break/Abort		*/
#define	R0_UDRUN	0x40		/* Tx Underrun/EOM	*/
#define	R0_CTS		0x20		/* CTS			*/
#define	R0_SYNC		0x10		/* Sync/Hunt		*/
#define	R0_DCD		0x08		/* DCD			*/
#define	R0_TxBE		0x04		/* Tx buffer empty	*/
#define	R0_ZERO		0x02		/* Zero count		*/
#define	R0_RxCA		0x01		/* Rx char. available	*/

#define	R1_EOF		0x80		/* End Of Frame (SDLC)	*/
#define	R1_CRC		0x40		/* CRC/Framing Error	*/
#define	R1_OVRUN	0x20		/* Rx Overrun		*/
#define	R1_PARITY	0x10		/* Parity Error		*/
#define	R1_RESID	0x0e		/* Residue code		*/
#define	R1_SENT		0x01		/* All sent		*/

#define	R3_RxA		0x20		/* Channel A Rx IP	*/
#define	R3_TxA		0x10		/* Channel A Tx IP	*/
#define	R3_EXTA		0x08		/* Channel A EXT/STAT	*/
#define	R3_RxB		0x20		/* Channel B Rx IP	*/
#define	R3_TxB		0x10		/* Channel B Tx IP	*/
#define	R3_EXTB		0x08		/* Channel B EXT/STAT	*/

#define	R10_ONEC	0x80		/* One clock missing	*/
#define	R10_TWOC	0x40		/* Two clock missing	*/
#define	R10_LOOP	0x10		/* Loop Sending		*/
#define	R10_ONLOOP	0x02		/* On Loop		*/

#define	R15_BREAK	0x80		/* Break/Abort IE	*/
#define	R15_UDRUN	0x40		/* Tx Underrun IE	*/
#define	R15_CTS		0x20		/* CTS IE		*/
#define	R15_SYNC	0x10		/* Sync/Hunt IE		*/
#define	R15_DCD		0x08		/* DCD IE		*/
#define	R15_ZERO	0x02		/* Zero count IE	*/

/*
 *	SCC write register
 */

#define	WR0		0
#define	WR1		1
#define	WR2		2
#define	WR3		3
#define	WR4		4
#define	WR5		5
#define	WR6		6
#define	WR7		7
#define	WR9		9
#define	WR10		10
#define	WR11		11
#define	WR12		12
#define	WR13		13
#define	WR14		14
#define	WR15		15

#define	W0_RES_UDRUN	0xc0		/* Reset Tx Underrun/EOM	*/
#define	W0_RES_TxCRC	0x80		/* Reset Tx CRC generator	*/
#define	W0_RES_RxCRC	0x40		/* Reset Rx CRC checker		*/
#define	W0_RES_IUS	0x38		/* Reset Highest IUS		*/
#define	W0_RES_ERROR	0x30		/* Error reset			*/
#define	W0_RES_TxINT	0x28		/* Reset TxINT pending		*/
#define	W0_RxINTE	0x20		/* Enable RxINT on next char.	*/
#define	W0_SND_ABORT	0x18		/* Send Abort (SDLC)		*/
#define	W0_RES_EXT	0x10		/* Reset EXT/STAT interrupts	*/

#define	W1_EN_WAIT	0x80		/* WAIT/DMA request enable	*/
#define	W1_WAIT_FUNC	0x40		/* WAIT/DMA request function	*/
#define	W1_WAIT_REQ	0x20		/* WAIT/DMA request on Rx/Tx	*/

#define	W1_RxINT_SC	0x18		/* Rx INT on special condition	*/
#define	W1_RxINT_ALL	0x10		/* Rx INT on all character	*/
#define	W1_RxINT_FRST	0x08		/* Rx INT on first character	*/
#define	W1_PARITY	0x04		/* Parity is special condition	*/
#define	W1_TxINTE	0x02		/* Tx INT enable		*/
#define	W1_EXTINTE	0x01		/* EXT INT enable		*/

#define	W3_Rx8BIT	0xc0		/* Rx 8 bits/character		*/
#define	W3_Rx6BIT	0x80		/* Rx 6 bits/character		*/
#define	W3_Rx7BIT	0x40		/* Rx 7 bits/character		*/
#define	W3_Rx5BIT	0x00		/* Rx 5 bits/character		*/

#define	W3_AUTO		0x20		/* Auto enable			*/
#define	W3_HUNT		0x10		/* Enter Hunt mode		*/
#define	W3_RxCRC	0x08		/* Rx CRC enable		*/
#define	W3_ADDR		0x04		/* Address search mode (SDLC)	*/
#define	W3_SYNCI	0x02		/* Sync char. load inhibit	*/
#define	W3_RxE		0x01		/* Rx enable			*/

#define	W4_X64		0xc0		/* X64 clock mode		*/
#define	W4_X32		0x80		/* X32 clock mode		*/
#define	W4_X16		0x40		/* X16 clock mode		*/
#define	W4_X1		0x00		/* X1 clock mode		*/

#define	W4_EXTSYNC	0x30		/* External Sync mode		*/
#define	W4_SDLC		0x20		/* SDLC mode			*/
#define	W4_SYNC16	0x10		/* 16 bit sync character	*/
#define	W4_SYNC8	0x00		/* 8 bit sync character		*/

#define	W4_STOP2	0x0c		/* 2 stop bits/character	*/
#define	W4_STOP1_5	0x08		/* 1.5 stop bits/character	*/
#define	W4_STOP1	0x04		/* 1 stop bit/character		*/
#define	W4_SYNC		0x00		/* Sync mode enable		*/

#define	W4_EVEN		0x02		/* Parity Even			*/
#define	W4_PARITY	0x01		/* Parity enable		*/

#define	W5_DTR		0x80		/* DTR				*/

#define	W5_Tx8BIT	0x60		/* Tx 8 bits/character		*/
#define	W5_Tx6BIT	0x40		/* Tx 6 bits/character		*/
#define	W5_Tx7BIT	0x20		/* Tx 7 bits/character		*/
#define	W5_Tx5BIT	0x00		/* Tx 5 bits/character		*/

#define	W5_BREAK	0x10		/* Send Break			*/
#define	W5_TxE		0x08		/* Tx enable			*/
#define	W5_CRC16	0x04		/* SDLC/CRC-16			*/
#define	W5_RTS		0x02		/* RTS				*/
#define	W5_TxCRC	0x01		/* Tx CRC enable		*/

#define	W9_RESET	0xc0		/* Force hardware reset		*/
#define	W9_RES_A	0x80		/* Channel reset A		*/
#define	W9_RES_B	0x40		/* Channel reset B		*/

#define	W9_STAT_HIGH	0x10		/* Staus High/Low		*/
#define	W9_MIE		0x08		/* Master Int. enable		*/
#define	W9_DLC		0x04		/* Disable lower chain		*/
#define	W9_NV		0x02		/* Non Vector			*/
#define	W9_VIS		0x01		/* Vector Include Status	*/

#define	W10_CRC_PRESET	0x80		/* CRC preset I/O		*/

#define	W10_FM0		0x60		/* FM0 (transition = 0)		*/
#define	W10_FM1		0x40		/* FM1 (transition = 1)		*/
#define	W10_NRZI	0x20		/* NRZI				*/
#define	W10_NRZ		0x00		/* NRZ				*/

#define	W10_POLL	0x10		/* Go active on poll		*/
#define	W10_MARK	0x08		/* Mark/Flag idle		*/
#define	W10_ABORT	0x04		/* Abort/Flag on underrun	*/
#define	W10_LOOP	0x02		/* Loop mode			*/
#define	W10_SYNC6	0x01		/* 6 bit/8 bit sync		*/

#define	W11_RTxC_XTAL	0x80		/* RTxC Xtal			*/

#define	W11_RxC_DPLL	0x60		/* RxC = DPLL output		*/
#define	W11_RxC_BRG	0x40		/* RxC = BR Gen. output		*/
#define	W11_RxC_TRxC	0x20		/* RxC = TRxC pin		*/
#define	W11_RxC_RTxC	0x00		/* RxC = RTxC pin		*/

#define	W11_TxC_DPLL	0x18		/* TxC = DPLL output		*/
#define	W11_TxC_BRG	0x10		/* TxC = BR Gen. output		*/
#define	W11_TxC_TRxC	0x08		/* TxC = TRxC pin		*/
#define	W11_TxC_RTxC	0x00		/* TxC = RTxC pin		*/

#define	W11_TRxC_O	0x04		/* TRxC O/I			*/

#define	W11_TRxC_DPLL	0x03		/* TRxC = DPLL output		*/
#define	W11_TRxC_BRG	0x02		/* TRxC = BR Gen output		*/
#define	W11_TRxC_TxC	0x01		/* TRxC = Transmit clock	*/
#define	W11_TRxC_XTAL	0x00		/* TRxC = Xtal output		*/

#define	W14_NRZI	0xe0		/* Set NRZI mode		*/
#define	W14_FM		0xc0		/* Set FM mode			*/
#define	W14_RTxC	0xa0		/* Set source = RTxC		*/
#define	W14_BRG		0x80		/* Set source = BR Gen.		*/
#define	W14_DIS_DPLL	0x60		/* Disable DPLL			*/
#define	W14_RES_CLK	0x40		/* Reset missing clock		*/
#define	W14_SEARCH	0x20		/* Enter search mode		*/

#define	W14_LOCAL	0x10		/* Local loopback mode		*/
#define	W14_ECHO	0x08		/* Auto echo			*/
#define	W14_DTR		0x04		/* DTR/Request function		*/
#define	W14_BRG_SRC	0x02		/* BR Gen. source		*/
#define	W14_BRGE	0x01		/* BR Gen. enable		*/

#define	W15_BREAK	0x80		/* Break/Abort IE		*/
#define	W15_UDRUN	0x40		/* Tx underrun/EOM IE		*/
#define	W15_CTS		0x20		/* CTS IE			*/
#define	W15_SYNC	0x10		/* Sync/Hunt IE			*/
#define	W15_DCD		0x08		/* DCD IE			*/
#define	W15_ZERO	0x02		/* Zero count IE		*/

#endif /* _SCCREG_ */
