/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratories.
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
 *	@(#)sdtrace.h	8.1 (Berkeley) 6/10/93
 *
 * from: $Header: sdtrace.h,v 1.6 92/12/02 03:53:47 torek Exp $ (LBL)
 */

/*
 * SCSI disk command tracing
 */

#if defined(SDTRACE) || !defined(KERNEL)
struct sdtrace {
	struct	timeval time;	/* timestamp */
	u_int	block;		/* disk block */
	u_int	bcount;		/* # bytes transferred */
	u_char	tcode;		/* trace code */
	u_char	target;		/* target number */
	u_char	unit;		/* unit number on target */
	u_char	read;		/* read operation */
};

#define	T_START		0x01
#define	T_MKCDB		0x02
#define	T_INTR		0x03
#endif

#ifdef SDTRACE
/* Allow kernel config to override number of entries */
#ifndef NSDOPBUF
#define	NSDOPBUF 1024
#endif

struct	sdtrace sdopbuf[NSDOPBUF];
struct	sdtrace *sdopptr = sdopbuf;
int	nsdopbuf = NSDOPBUF;	/* for sdtrace */
u_long	sdopcnt;

#define	SD_TRACE(code, sc, bp) { \
	if (++sdopptr >= &sdopbuf[NSDOPBUF]) \
		sdopptr = sdopbuf; \
	microtime(&sdopptr->time); \
	sdopptr->tcode = code; \
	sdopptr->read = bp->b_flags & B_READ; \
	sdopptr->block = bp->b_blkno; \
	sdopptr->bcount = bp->b_bcount; \
	sdopptr->target = sc->sc_unit.u_targ; \
	sdopptr->unit = sc->sc_unit.u_unit; \
	++sdopcnt; \
}
#else
#define	SD_TRACE(code, sc, bp) { }
#endif
