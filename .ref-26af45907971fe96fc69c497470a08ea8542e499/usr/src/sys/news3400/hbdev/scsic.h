/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: scsic.h,v 4.300 91/06/09 06:22:24 root Rel41 $ SONY
 *
 *	@(#)scsic.h	7.1 (Berkeley) %G%
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
