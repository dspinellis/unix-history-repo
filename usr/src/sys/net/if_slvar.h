/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)if_slvar.h	7.8 (Berkeley) %G%
 *
 * $Header: if_slvar.h,v 1.3 89/05/31 02:25:18 van Exp $
 */

/*
 * Definitions for SLIP interface data structures
 * 
 * (This exists so programs like slstats can get at the definition
 *  of sl_softc.)
 */
struct sl_softc {
	struct	ifnet sc_if;		/* network-visible interface */
	struct	ifqueue sc_fastq;	/* interactive output queue */
	struct	tty *sc_ttyp;		/* pointer to tty structure */
	u_char	*sc_mp;			/* pointer to next available buf char */
	u_char	*sc_ep;			/* pointer to last available buf char */
	u_char	*sc_buf;		/* input buffer */
	u_int	sc_flags;		/* see below */
	u_int	sc_escape;	/* =1 if last char input was FRAME_ESCAPE */
	u_int	sc_bytessent;
	u_int	sc_bytesrcvd;
	long	sc_lasttime;		/* last time a char arrived */
	long	sc_starttime;		/* last time a char arrived */
	long	sc_abortcount;		/* number of abort esacpe chars */
#ifdef INET				/* XXX */
	struct	slcompress sc_comp;	/* tcp compression data */
#endif
};

/* visible flags */
#define	SC_COMPRESS	IFF_LLC0	/* compress TCP traffic */
#define	SC_NOICMP	IFF_LLC1	/* supress ICMP traffic */
#define	SC_AUTOCOMP	IFF_LLC2	/* auto-enable TCP compression */
/* internal flags (should be separate) */
#define	SC_ABORT	0x10000		/* have been sent an abort request */

/* this stuff doesn't belong here... */
#define	SLIOCGFLAGS	_IOR('t', 90, int)	/* get configuration flags */
#define	SLIOCSFLAGS	_IOW('t', 89, int)	/* set configuration flags */
#define	SLIOCGUNIT	_IOR('t', 88, int)	/* get slip unit number */
