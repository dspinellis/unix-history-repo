/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)if_slvar.h	7.10 (Berkeley) %G%
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
	long	sc_lasttime;		/* last time a char arrived */
	long	sc_abortcount;		/* number of abort esacpe chars */
	long	sc_starttime;		/* time of first abort in window */
#ifdef INET				/* XXX */
	struct	slcompress sc_comp;	/* tcp compression data */
#endif
};

/* visible flags */
#define	SC_COMPRESS	IFF_LINK0	/* compress TCP traffic */
#define	SC_NOICMP	IFF_LINK1	/* supress ICMP traffic */
#define	SC_AUTOCOMP	IFF_LINK2	/* auto-enable TCP compression */

/* this stuff doesn't belong here... */
#define	SLIOCGUNIT	_IOR('t', 88, int)	/* get slip unit number */

#ifdef KERNEL
void	 slattach __P((void));
void	 slclose __P((struct tty *));
void	 slinput __P((int, struct tty *));
int	 slioctl __P((struct ifnet *, int, caddr_t));
int	 slopen __P((dev_t, struct tty *));
int	 sloutput __P((struct ifnet *,
	    struct mbuf *, struct sockaddr *, struct rtentry *));
void	 slstart __P((struct tty *));
int	 sltioctl __P((struct tty *, int, caddr_t, int));
#endif
