/*
 * if_ppp.h - Point-to-Point Protocol definitions.
 *
 * Copyright (c) 1989 Carnegie Mellon University.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by Carnegie Mellon University.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * Modified by Paul Mackerras (paulus@cs.anu.edu.au)
 * PPP_MRU added, PPP_MTU changed to 296 (default only), added sc_outm.
 *
 *	$Id$
 *	From: if_ppp.h,v 1.3 1993/08/09 02:37:32 paulus Exp
 */

/*
 * Standard PPP header.
 */
struct ppp_header {
	u_char	ph_address;	/* Address Field */
	u_char	ph_control;	/* Control Field */
	u_short	ph_protocol;	/* Protocol Field */
};

#define	PPP_ALLSTATIONS	0xff	/* All-Stations broadcast address */
#define	PPP_UI		0x03	/* Unnumbered Information */
#define	PPP_FLAG	0x7e	/* Flag Sequence */
#define	PPP_ESCAPE	0x7d	/* Asynchronous Control Escape */
#define	PPP_TRANS	0x20	/* Asynchronous transparency modifier */

/*
 * Protocol types.
 */
#define PPP_IP		0x21	/* Internet Protocol */
#define	PPP_XNS		0x25	/* Xerox NS */
#define	PPP_VJC_COMP	0x2d	/* VJ compressed TCP */
#define	PPP_VJC_UNCOMP	0x2f	/* VJ uncompressed TCP */
#define PPP_LCP		0xc021	/* Link Control Protocol */

/*
 * Important FCS values.
 */
#define PPP_INITFCS	0xffff	/* Initial FCS value */
#define PPP_GOODFCS	0xf0b8	/* Good final FCS value */
#define PPP_FCS(fcs, c)	(((fcs) >> 8) ^ fcstab[((fcs) ^ (c)) & 0xff])

#define	PPP_MTU		296	/* Default MTU (size of Info field) */
#define PPP_MRU		1500	/* Default MRU (max receive unit) */
#define	PPP_HIWAT	400	/* Don't start a new packet if HIWAT on que */

struct ppp_softc {
	struct ifnet sc_if;	/* network-visible interface */
	u_int	sc_flags;	/* see below */
	struct	tty *sc_ttyp;	/* pointer to tty structure */
	struct	mbuf *sc_outm;	/* mbuf chain being output currently */
	struct	mbuf *sc_m;	/* pointer to input mbuf chain */
	struct	mbuf *sc_mc;	/* pointer to current input mbuf */
	char	*sc_mp;		/* pointer to next char in input mbuf */
	short	sc_ilen;	/* length of input-packet-so-far */
	u_short	sc_fcs;		/* FCS so far */
	u_long	sc_asyncmap;	/* async control character map */
	struct	ifqueue sc_inq;	/* TTY side input queue */
#ifdef	VJC
	struct	slcompress sc_comp; /* vjc control buffer */
#endif
	u_int	sc_bytessent;
	u_int	sc_bytesrcvd;
};

/* flags */
#define	SC_ESCAPED	0x00010000	/* saw a PPP_ESCAPE */
#define	SC_FLUSH	0x00020000	/* flush input until next PPP_FLAG */
#define SC_COMP_PROT	0x00000001	/* protocol compression */
#define SC_COMP_AC	0x00000002	/* header compression */
#define	SC_COMP_TCP	0x00000004	/* TCP traffic (VJ) compression */

#define t_sc T_LINEP

/* this stuff doesn't belong here... */
#define	PPPIOCGFLAGS	_IOR('t', 90, int)	/* get configuration flags */
#define	PPPIOCSFLAGS	_IOW('t', 89, int)	/* set configuration flags */
#define	PPPIOCGASYNCMAP	_IOR('t', 88, int)	/* get async map */
#define	PPPIOCSASYNCMAP	_IOW('t', 87, int)	/* set async map */
#define	PPPIOCGUNIT	_IOR('t', 86, int)	/* get ppp unit number */

/* old copies of PPP may have defined this */
#if !defined(ifr_mtu)
#define ifr_mtu	ifr_metric
#endif

