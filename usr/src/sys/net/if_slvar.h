/*	@(#)if_slvar.h	7.4 (Berkeley) %G% */

/*
 * Definitions for SLIP interface data structures
 * 
 * (this exists so programs like slstats can get at the definition
 *  of sl_softc.)
 *
 * $Header: if_slvar.h,v 1.3 89/05/31 02:25:18 van Exp $
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
#define	SC_COMPRESS	0x0002		/* compress TCP traffic */
#define	SC_NOICMP	0x0004		/* supress ICMP traffic */
#define	SC_AUTOCOMP	0x0008		/* auto-enable TCP compression */
/* internal flags (should be separate) */
#define	SC_ABORT	0x10000		/* have been sent an abort request */

/* this stuff doesn't belong here... */
#define	SLIOCGFLAGS	_IOR('t', 90, int)	/* get configuration flags */
#define	SLIOCSFLAGS	_IOW('t', 89, int)	/* set configuration flags */
#define	SLIOGUNIT	_IOW('t', 88, int)	/* get slip unit number */
