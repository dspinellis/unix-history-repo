/*
 * Copyright (c) 1982, 1986, 1988 Regents of the University of California.
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
 *	@(#)if_dpreg.h	7.2 (Berkeley) 8/30/90
 */

/*
 * DPV-11 Registers and bits.
 */
struct dpdevice {
	short	dprcsr;		/* Receive Control and Status Register */
	union	{
		short	u_rdsr;	/* Receive Data and Status Reg, rd only */
		short	u_sar;	/* Parameter Control Sync/Address Reg, wr only*/
	}	dpun;
	short	dpclr;		/* Parameter Control/Character Length Reg */
	short	dptdsr;		/* Transmit Data and Status Register */
};

#define dprdsr	dpun.u_rdsr
#define dpsar	dpun.u_sar


/* bits in dprcsr */
#define DP_RL	0x0001		/* Remote Loopback Mode */
#define DP_DTR	0x0002		/* Data Terminal Ready (modem) */
#define DP_RTS	0x0004		/* Request to Send (modem) */
#define DP_LL	0x0008		/* Local Loopback */
#define DP_RE	0x0010		/* Enable Receiver */
#define DP_MIE	0x0020		/* Modem (Change) Interrupt Enable */
#define DP_RIE	0x0040		/* Receiver Interrupt Enable */
#define DP_RDR	0x0080		/* Receiver Data Ready */
#define DP_SFD	0x0100		/* Sync or Flag Detected */
#define DP_DSR	0x0200		/* Data Set Ready (modem) */
#define DP_RSR	0x0400		/* Receiver Status Ready (attention) */
#define DP_RA	0x0800		/* Receiver Active (receiving data) */
#define DP_RR	0x1000		/* Receiver Ready (modem) */
#define DP_CTS	0x2000		/* Clear to Send (modem) */
#define DP_IC	0x4000		/* Incoming Call */
#define DP_MSC	0x8000		/* Modem Status Change (CTS, RR, IC, DM) */

/* flags for modem-control */
#define	DP_ON	DP_DTR
#define	DP_OFF	0

/* bits in dprdsr */
#define DP_RBUF	0x00ff		/* Received Data */
#define DP_RSM	0x0100		/* Receiver Start of Message */
#define DP_REM	0x0200		/* Receiver End of Message */
#define DP_RGA	0x0400		/* Receiver Go-Ahead or Abort */
#define DP_ROVR	0x0800		/* Receiver Receiver Over-Run */
#define DP_RABC	0x7000		/* Reciever Assembled Bit Count */
#define DP_REC	0x8000		/* Reciever Error Check */

/* bits in dpsar */
#define DP_SYNC	0x00ff		/* Sync Char or Station Address */
#define DP_EM	0x0700		/* Error Detection Mode */
#define DP_IDLE	0x0800		/* Idle Mode Select */
#define DP_SAM	0x1000		/* Secondary Address Mode */
#define DP_SSLM	0x2000		/* Strip Synch/Loop Mode */
#define DP_CHRM	0x4000		/* Character/Bit Mode */
#define DP_APA	0x8000		/* Recognize All Parties Addressed */

/* bits in dpclr */
#define	DP_CLR	0x0001		/* Reset DP */
#define	DP_XA	0x0002		/* Transmiter Active (transmitting data) */
#define	DP_XBE	0x0004		/* Transmit Buffer Available */
#define	DP_ILB	0x0008		/* Internal Loopback (Maintenance Mode) */
#define	DP_XE	0x0010		/* Transmit Enable */
#define	DP_SQTM	0x0020		/* Signal Quality/Test Mode */
#define	DP_XIE	0x0040		/* Transmit Interrupt Enable */
#define	DP_MBZ1	0x0080		/* Reserved (must be zero) */
#define DP_RCL	0x0700		/* Receive Character Length */
#define DP_RL8	0x0000		/* RCL = 8 bits */
#define DP_RL7	0x0700		/* RCL = 7 bits */
#define DP_RL6	0x0600		/* RCL = 6 bits */
#define DP_ECF	0x0800		/* Extended Control Field Option */
#define DP_EAF	0x1000		/* Extended Address Field Option */
#define DP_XCL	0xE000		/* Transmit Character Length */
#define DP_XL8	0x0000		/* XCL = 8 bits */
#define DP_XL7	0xE000		/* XCL = 7 bits */
#define DP_XL6	0xC000		/* XCL = 6 bits */


/* bits in dptdsr */
#define DP_XBUF 0x00FF		/* data to be transmitted */
#define DP_XSM	0x0100		/* Transmit Start of Message */
#define DP_XEM	0x0200		/* Transmit End of Message */
#define DP_XABO	0x0400		/* Tranmsit Abort */
#define DP_XGA	0x0800		/* Tranmsit Go - Ahead */
#define DP_MBZ2	0x7000		/* Reserved (must be zero) */
#define DP_XERR	0x8000		/* Data Late -- XBUF not serviced in time */

#define DP_MTU	2048		/* Very Big X.25 data, normally 128 */
