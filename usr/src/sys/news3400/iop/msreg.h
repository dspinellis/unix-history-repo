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
 * from: $Hdr: msreg.h,v 4.300 91/06/09 06:43:00 root Rel41 $ SONY
 *
 *	@(#)msreg.h	8.1 (Berkeley) 6/11/93
 */

/*
 * mouse register structure definition.
 */

#ifndef __MSREG__
#define __MSREG__ 1

#ifdef KERNEL

struct msreg {
/*00*/	u_char		ms_control;	/* CPU/IOP */
#define		MS_DONE		0x01	/* IOP has completed command from CPU */
#define		MS_EVREADY	0x02	/* IOP has made event report ready */
/*01*/	u_char		ms_command;	/* CPU */
#define		MS_CPROBE	0
#define		MS_CATTACH	1
#define		MS_CSAMPLE	2	/* requset current value */
#define		MS_CSETEM	3	/* set event mask */
#define		MS_CSETXY	4	/* set current coordinate */
#define		MS_CSETPARAM	5	/* set mouse parameter (mag, delta) */
#define		MS_CQFLUSH	6	/* flush event queue */
#define		MS_CSETRANGE	7	/* set coordinate range */
/*02*/	u_char		ms_result;	/* IOP */
#define		MS_ROK		0	/* done */
#define		MS_RERROR	1	/* failed */
#define		MS_REVENT	2	/* event has occured */
/*03*/	u_char		ms_eventmask;	/* CPU */
/*04*/	struct ms_param	ms_param;	/* CPU */
/*0c*/	struct ms_coord	ms_coord;	/* CPU */
/*14*/	struct ms_data	ms_data;	/* IOP */
/*20*/	struct ms_event	ms_event;	/* IOP */
/*38*/	int		ms_unit;	/* CPU */
/*3c*/	int		ms_sevcount;	/*XXX*/
/*40*/	struct ms_range	ms_range;	/* CPU */
};

#define MS_S_BYTE	0		/* start byte */
#define MS_X_BYTE	1		/* second byte */
#define MS_Y_BYTE	2		/* third byte */
#define MS_DB_SIZE	3

#define MS_S_MARK	0x80		/* start mark (first byte)*/
#define MS_S_X7		0x08		/* MSB(sign bit) of X */
#define MS_S_Y7		0x10		/* MSB(sign bit) of Y */
#define MS_S_SW1	0x01		/* left button is pressed */
#define MS_S_SW2	0x02		/* right button is pressed */
#define MS_S_SW3	0x04		/* middle button is pressed */

#define MS_X_X06	0x7f		/* data bits of X (second byte) */
#define MS_Y_Y06	0x7f		/* data bits of Y (third byte) */

struct ms_stat {
	int		mss_stat;
#define		MS_ACTIVE       0x01
#define		MS_WAIT         0x02    /* waiting for command completion */
#define		MS_EVWAIT       0x04    /* waiting for event report */
#define		MS_RCOLL        0x08    /* select collsion on read */
#define		MS_WCOLL        0x10    /* select collsion on write */
#define		MS_NBIO         0x20    /* noblocked read write */
#define		MS_ASYNC        0x40    /* if event queued then send SIGIO */
	int		mss_pgrp;
	int		mss_mode;
	int		mss_command;
	int		mss_eventmask;
	struct ms_data	mss_data;
	struct ms_data	mss_data_old;
	struct ms_param	mss_param;
	struct ms_range	mss_range;
	struct proc	*mss_rsel;
	struct proc	*mss_wsel;
	struct ms_queue	*mss_queue;
};

#define		MSPRI		(PZERO+1)
#define		MSUNIT(dev)	(minor(dev)&017)
#define		MSOLDIF(dev)    (minor(dev)&020)

#endif /* KERNEL */
#endif /* !__MSREG__ */
