/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)spp_var.h	6.2 (Berkeley) %G%
 */

/*
 * Sp control block, one per connection
 */
struct sppcb {
	struct	spidp_q	s_q;		/* queue for out-of-order receipt */
	struct	nspcb	*s_nspcb;	/* backpointer to internet pcb */
	u_char	s_state;
	u_char	s_flags;
#define SF_AK	0x01			/* Acknowledgement requested */
#define SF_DELACK 0x02			/* Ak, waiting to see if we xmit*/
#define SF_HI	0x04			/* Show headers on input */
#define SF_HO	0x08			/* Show headers on output */
#define SF_PI	0x10			/* Packet (datagram) interface */
	u_short s_mtu;			/* Max packet size for this stream */
/* use sequence fields in headers to store sequence numbers for this
   connection */
	struct	spidp s_shdr;		/* prototype header to transmit */
#define s_cc s_shdr.si_cc		/* connection control (for EM bit) */
#define s_dt s_shdr.si_dt		/* datastream type */
#define s_sid s_shdr.si_sid		/* source connection identifier */
#define s_did s_shdr.si_did		/* destination connection identifier */
#define s_seq s_shdr.si_seq		/* sequence number */
#define s_ack s_shdr.si_ack		/* acknowledge number */
#define s_alo s_shdr.si_alo		/* allocation number */
#define s_dport s_shdr.si_dna.x_port	/* where we are sending */
	struct sphdr s_rhdr;		/* last received header (in effect!)*/
	u_short s_rack;			/* their acknowledge number */
	u_short s_ralo;			/* their allocation number */
	u_short s_snt;			/* highest packet # we have sent */

/* timeout stuff */
	short	s_idle;			/* time idle */
	short	s_timer[TCPT_NTIMERS];	/* timers */
	short	s_rxtshift;		/* log(2) of rexmt exp. backoff */
	u_short	s_rtseq;		/* packet being timed */
	short	s_rtt;			/* timer for round trips */
	short	s_srtt;			/* averaged timer */
	char	s_force;		/* which timer expired */

/* out of band data */
	char	s_oobflags;
#define SF_SOOB	0x08			/* sending out of band data */
#define SF_IOOB 0x10			/* receiving out of band data */
	char	s_iobc;			/* input characters */
/* debug stuff */
	u_short	s_want;			/* Last candidate for sending */
};

#define	nstosppcb(np)	((struct sppcb *)(np)->nsp_pcb)
#define	sotosppcb(so)	(nstosppcb(sotonspcb(so)))

struct	spp_istat {
	short	hdrops;
	short	badsum;
	short	badlen;
	short	slotim;
	short	fastim;
	short	nonucn;
	short	noconn;
	short	notme;
	short	wrncon;
	short	bdreas;
	short	gonawy;
};

#ifdef KERNEL
struct spp_istat spp_istat;
u_short spp_iss;
extern struct sppcb *spp_close(), *spp_disconnect(),
	*spp_usrclosed(), *spp_timers(), *spp_drop();
#endif

#define	SPP_ISSINCR	128
/*
 * SPP sequence numbers are 16 bit integers operated
 * on with modular arithmetic.  These macros can be
 * used to compare such integers.
 */
#define	SSEQ_LT(a,b)	(((short)((a)-(b))) < 0)
#define	SSEQ_LEQ(a,b)	(((short)((a)-(b))) <= 0)
#define	SSEQ_GT(a,b)	(((short)((a)-(b))) > 0)
#define	SSEQ_GEQ(a,b)	(((short)((a)-(b))) >= 0)
