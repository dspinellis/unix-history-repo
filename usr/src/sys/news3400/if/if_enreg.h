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
 * from: $Hdr: if_enreg.h,v 4.300 91/06/09 06:25:57 root Rel41 $ SONY
 *
 *	@(#)if_enreg.h	8.1 (Berkeley) 6/11/93
 */

/*
 * if_enreg.h for news800, news900
 */

#ifndef __IF_ENREG__
#define __IF_ENREG__ 1

/* command definitions */
#define	ENC_ADDR	0x00		/* Get Ethernet address */
#define	ENC_DIAG	0x01		/* Run On-board Diagnostics */
#define	ENC_START	0x02		/* Go Online */
#define	ENC_RESET	0x03		/* Reset */
#define	ENC_STAT	0x04		/* Report and Reset Statistics */
#define	ENC_XMIT	0x05		/* Load Transmit Data and Send */
#define	ENC_RECV	0x06		/* Receive Data */
#define	ENC_PROM	0x07		/* Set prom mode */
#define	ENC_SADDR	0x08		/* Set Ethernet address */
#define	ENC_NEWIF	0x09		/* Switch driver interface */

#endif /* !__IF_ENREG__ */
