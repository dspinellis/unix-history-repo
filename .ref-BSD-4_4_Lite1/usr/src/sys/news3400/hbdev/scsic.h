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
 * from: $Hdr: scsic.h,v 4.300 91/06/09 06:22:24 root Rel41 $ SONY
 *
 *	@(#)scsic.h	8.1 (Berkeley) 6/11/93
 */

/*
 * Copyright (c) 1987- by SONY Corporation.
 */

/*
 *	scsic.h	ver 1.1		
 *
 *		Header file for scsi.c.
 */

#define SCSI_NOTWAIT	0x2		/* don't wait cmd completion */

#define	splhi		spl7		/* IPL 7  Be careful to use this.*/

/*
 * Delay units are in microseconds.
 *
 *	1ms = 1000  on news800
 *	so 1ms = 1500 on 25MHz
 */
#define HDELAY(n)				\
	{					\
		register int N = (n)*1500 ;	\
		while(--N > 0);			\
	}

struct scsi_stat {
	int	wbc;	/* # of channel that is waiting for scsi bus free */	
	int	wrc;	/* # of channel that is waiting for reselection */	
	struct sc_chan_stat *ip;
			/* In progress channel. Same as ISTAT.IP */
	int	ipc;		/* number of in progress channel. */
	int	dma_stat;	/* OFF = DMAC is not used */
#define SC_DMAC_RD	1
#define SC_DMAC_WR	2
};

#undef VOLATILE
#ifdef mips
#define VOLATILE volatile
#else
#define VOLATILE
#endif

struct sc_chan_stat {
	struct scsi	*sc ;		/* scsi struct address */
	u_char		comflg;		/* flag for save comand pointer */
	u_int		stcnt;		/* save transfer count */
	u_char		*spoint;	/* save transfer point */
	u_int		stag;		/* save tag register */
	u_int		soffset;	/* save offset register */
	u_char		intr_flg;	/* interrupt flag. SCSI_INTEN/INTDIS */
	int		chan_num;	/* channel NO. */
	VOLATILE struct sc_chan_stat *wb_next;	/* wait bus channel queue */
};

#undef VOLATILE

extern struct scintsw scintsw[];

extern struct scsi_stat scsi_stat;
