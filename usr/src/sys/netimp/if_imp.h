/*
 * Copyright (c) 1982, 1986, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)if_imp.h	7.7 (Berkeley) 6/28/90
 */

/*
 * Structure of IMP 1822 long leader.
 */
struct control_leader {
	u_char	dl_format;	/* 1-8   leader format */
	u_char	dl_network;	/* 9-16  src/dest network */
	u_char	dl_flags;	/* 17-24 leader flags */
	u_char	dl_mtype;	/* 25-32 message type */
	u_char	dl_htype;	/* 33-40 handling type */
	u_char	dl_host;	/* 41-48 host number */
	u_short	dl_imp;		/* 49-64 imp field */
	u_char	dl_link;	/* 65-72 link number */
	u_char	dl_subtype;	/* 73-80 message subtype */
};

struct imp_leader {
	struct	control_leader il_dl;
#define	il_format	il_dl.dl_format
#define	il_network	il_dl.dl_network
#define	il_flags	il_dl.dl_flags
#define	il_mtype	il_dl.dl_mtype
#define	il_htype	il_dl.dl_htype
#define	il_host		il_dl.dl_host
#define	il_imp		il_dl.dl_imp
#define	il_link		il_dl.dl_link
#define	il_subtype	il_dl.dl_subtype
	u_short	il_length;	/* message length */
};

#define	IMP_MAXHOSTMSG	8	/* max messages in flight to a host */
#define	IMP_NOOPCNT	3	/* # of noops to send imp on reset */
/* insure things are even... */
#define	IMPMTU		((8159 / NBBY) & ~01)
#define	IMP_RCVBUF	((8159 / NBBY + 2) & ~01)

/*
 * IMP-host flags
 */
#define	IMP_1822L_H2I	0xd	/* 1822L host-to-imp, 96-bit format */
#define	IMP_1822L_I2H	0xe	/* 1822L imp-to-host, 96-bit format */
#define	IMP_NFF		0xf	/* 96-bit (new) format */
#define	IMP_TRACE	0x8	/* trace message route */

/*
 * IMP-host message types.
 */
#define	IMPTYPE_DATA		0	/* data for protocol */
#define	IMPTYPE_BADLEADER	1	/* leader error */
#define	IMPTYPE_DOWN		2	/* imp going down */
#define	IMPTYPE_NOOP		4	/* noop seen during initialization */
#define	IMPTYPE_RFNM		5	/* request for new messages */
#define	IMPTYPE_HOSTDEAD	6	/* host doesn't respond */
#define	IMPTYPE_HOSTUNREACH	7	/* host unreachable */
#define	IMPTYPE_BADDATA		8	/* data error */
#define	IMPTYPE_INCOMPLETE	9	/* incomplete message, send rest */
#define	IMPTYPE_RESET		10	/* reset complete */
/* non-blocking IMP interface */
#define	IMPTYPE_RETRY		11	/* IMP refused, try again */
#define	IMPTYPE_NOTIFY		12	/* IMP refused, will notify */
#define	IMPTYPE_TRYING		13	/* IMP refused, still rexmt'ng */
#define	IMPTYPE_READY		14	/* ready for next message */

/*
 * Link numbers
 */
#define	IMPLINK_IP		155
#define	IMPLINK_LOWEXPER	156
#define	IMPLINK_HIGHEXPER	158

/*
 * IMPTYPE_DOWN subtypes, in link number field.
 */
#define	IMP_DMASK		0x3	/* host going down mask */
#define	IMPDOWN_GOING		0	/* 30 secs */
#define	IMPDOWN_PM		1	/* hardware PM */
#define	IMPDOWN_RELOAD		2	/* software reload */
#define	IMPDOWN_RESTART		3	/* emergency restart */
#define	IMPDOWN_WHENMASK	0x3c	/* mask for "how soon" */
#define	IMPDOWN_WHENSHIFT	2	/* shift for "how soon" */
#define	IMPDOWN_WHENUNIT	5	/* unit for "how soon", 5 min. */

#define	IMPTV_DOWN	30		/* going down timer 30 secs */

#ifdef IMPMESSAGES
/*
 * Messages from IMP regarding why
 * it's going down.
 */
char *impmessage[] = {
	"in 30 seconds",
	"for hardware PM",
	"to reload software",
	"for emergency reset"
};
#endif

/*
 * IMPTYPE_BADLEADER subtypes.
 */
#define	IMPLEADER_ERR		0	/* error flip-flop set */
#define	IMPLEADER_SHORT		1	/* leader < 80 bits */
#define	IMPLEADER_TYPE		2	/* illegal type field */
#define	IMPLEADER_OPPOSITE	3	/* opposite leader type */

/*
 * IMPTYPE_HOSTDEAD subtypes.
 */
#define	IMPHOST_NORDY		1	/* ready-line negated */
#define	IMPHOST_TARDY		2	/* tardy receiving mesgs */
#define	IMPHOST_NOEXIST		3	/* NCC doesn't know host */
#define	IMPHOST_IMPSOFT		4	/* IMP software won't allow mesgs */
#define	IMPHOST_PM		5	/* host down for scheduled PM */
#define	IMPHOST_HARDSCHED	6	/* " " " " hardware work */
#define	IMPHOST_SOFTSCHED	7	/* " " " " software work */
#define	IMPHOST_RESTART		8	/* host down for emergency restart */
#define	IMPHOST_POWER		9	/* down because of power outage */
#define	IMPHOST_BREAKPOINT	10	/* host stopped at a breakpoint */
#define	IMPHOST_HARDWARE	11	/* hardware failure */
#define	IMPHOST_NOTUP		12	/* host not scheduled to be up */
/* 13-14 currently unused */
#define	IMPHOST_COMINGUP	15	/* host in process of coming up */

/*
 * IMPTYPE_HOSTUNREACH subtypes.
 */
#define	IMPREACH_IMP		0	/* destination IMP can't be reached */
#define	IMPREACH_HOSTUP		1	/* destination host isn't up */
#define	IMPREACH_LEADER		2	/* host doesn't support long leader */
#define	IMPREACH_PROHIBITED	3	/* communication is prohibited */

/*
 * IMPTYPE_INCOMPLETE subtypes.
 */
#define	IMPCOMPLETE_SLOW	0	/* host didn't take data fast enough */
#define	IMPCOMPLETE_TOOLONG	1	/* message was too long */
#define	IMPCOMPLETE_TIMEOUT	2	/* mesg transmission time > 15 sec. */
#define	IMPCOMPLETE_FAILURE	3	/* IMP/circuit failure */
#define	IMPCOMPLETE_NOSPACE	4	/* no resources within 15 sec. */
#define	IMPCOMPLETE_IMPIO	5	/* src IMP I/O failure during receipt */

/*
 * IMPTYPE_RETRY subtypes.
 */
#define	IMPRETRY_BUFFER		0	/* IMP buffer wasn't available */
#define	IMPRETRY_BLOCK		1	/* connection block unavailable */

#define	RFNMTIMER	(120*PR_SLOWHZ)	 /* time to wait for RFNM for msg. */
#define	IMP_OTIMER	(5*IFNET_SLOWHZ) /* max output time unless blocked */

/*
 * Data structure shared between IMP protocol module and hardware
 * interface driver.  Used to allow layering of IMP routines on top
 * of varying device drivers.
 */
struct impcb {
	int	ic_hwunit;		/* H/W unit number */
	char	*ic_hwname;		/* H/W type name */
	char	ic_oactive;		/* output in progress */
	int	(*ic_init)();		/* hardware init routine */
	int	(*ic_output)();		/* hardware output routine */
	int	(*ic_down)();		/* hardware "drop ready" routine */
};

/*
 * IMP software status per interface.
 * (partially shared with the hardware specific module)
 *
 * Each interface is referenced by a network interface structure,
 * imp_if, which the routing code uses to locate the interface.
 * This structure contains the output queue for the interface, its
 * address, ...  IMP specific structures used in connecting the
 * IMP software modules to the hardware specific interface routines
 * are stored here.  The common structures are made visible to the
 * interface driver by passing a pointer to the hardware routine
 * at "attach" time.
 */
struct imp_softc {
	struct	ifnet imp_if;		/* network visible interface */
	struct	impcb imp_cb;		/* hooks to hardware module */
	int	imp_state;		/* current state of IMP */
	int	imp_dropcnt;		/* used during initialization */
	struct	mbuf *imp_hosts;	/* Head of host table hash chains. */
	struct	mbuf *imp_hostq;	/* current round-robin-output mark */
	u_int	imp_hostent;		/* current round-robin-output mark */
	int	imp_msgready;		/* number of messages ready to send */
	u_long	imp_block;		/* times imp blocked output */
	u_long	imp_lostrfnm;		/* rfnm's timed out */
	u_long	imp_badrfnm;		/* rfnm/incompl after timeout/bogus */
	u_long	imp_incomplete;		/* incomplete's received */
	u_long	imp_garbage;		/* bad messages received */
};

struct	imp_softc *impattach();

/*
 * State of an IMP.
 */
#define	IMPS_DOWN	0		/* unavailable, host not ready */
#define	IMPS_WINIT	1		/* imp not ready, waiting for init */
#define	IMPS_INIT	2		/* coming up */
#define	IMPS_UP		3		/* ready to go */
#define	IMPS_GOINGDOWN	4		/* been told we go down soon */

#define	IMPS_RUNNING(s)	((s) >= IMPS_UP)	/* ready for messages */
#define	IMPS_IMPREADY(s) ((s) >= IMPS_INIT)	/* IMP ready line on */

#ifdef IMPLEADERS
char *impleaders[IMPTYPE_READY+1] = {
	"DATA", "BADLEADER", "DOWN", "bad", "NOOP", "RFNM", "HOSTDEAD",
	"HOSTUNREACH", "BADDATA", "INCOMPLETE", "RESET", "RETRY",
	"NOTIFY", "TRYING", "READY"
};
#endif
