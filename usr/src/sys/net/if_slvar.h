/*	if_slvar.h	7.1	89/06/29	*/

/*
 * Definitions for SLIP "interface" data structure.
 *
 * (This exists so that programs can interpret the kernel data structures.)
 */
struct sl_softc {
	struct	ifnet sc_if;		/* network-visible interface */
	short	sc_flags;		/* see below */
	short	sc_ilen;		/* length of input-packet-so-far */
	struct	tty *sc_ttyp;		/* pointer to tty structure */
	char	*sc_mp;			/* pointer to next available buf char */
	char	*sc_buf;		/* input buffer */
	long	sc_lasttime;	/* last time a char arrived - seconds */
	long	sc_starttime;	/* last time a char arrived - seconds */
	long	sc_abortcount;	/* number of abort esacpe chars */
#ifdef INET
	struct	slcompress sc_comp;	/* tcp compression state */
#endif
};

/* flags */
#define	SC_ESCAPED	0x0001		/* saw a FRAME_ESCAPE */
#define	SC_COMPRESS	0x0002		/* compress TCP traffic */
#define	SC_NOICMP	0x0004		/* supress ICMP traffic */
#define	SC_ABORT	0x0008		/* have been sent an abort request */
#define	SC_OACTIVE	0x0010		/* output is active */

/* this stuff doesn't belong here... */
#define	SLIOCGFLAGS	_IOR('t', 90, int)	/* get configuration flags */
#define	SLIOCSFLAGS	_IOW('t', 89, int)	/* set configuration flags */
