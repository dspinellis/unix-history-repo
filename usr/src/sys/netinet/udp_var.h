/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)udp_var.h	7.6 (Berkeley) %G%
 */

/*
 * UDP kernel structures and variables.
 */
struct	udpiphdr {
	struct 	ipovly ui_i;		/* overlaid ip structure */
	struct	udphdr ui_u;		/* udp header */
};
#define	ui_next		ui_i.ih_next
#define	ui_prev		ui_i.ih_prev
#define	ui_x1		ui_i.ih_x1
#define	ui_pr		ui_i.ih_pr
#define	ui_len		ui_i.ih_len
#define	ui_src		ui_i.ih_src
#define	ui_dst		ui_i.ih_dst
#define	ui_sport	ui_u.uh_sport
#define	ui_dport	ui_u.uh_dport
#define	ui_ulen		ui_u.uh_ulen
#define	ui_sum		ui_u.uh_sum

struct	udpstat {
				/* input statistics: */
	int	udps_ipackets;		/* total input packets */
	int	udps_hdrops;		/* packet shorter than header */
	int	udps_badsum;		/* checksum error */
	int	udps_badlen;		/* data length larger than packet */
	int	udps_noport;		/* no socket on port */
	int	udps_noportbcast;	/* of above, arrived as broadcast */
	int	udps_fullsock;		/* not delivered, input socket full */
	int	udpps_pcbcachemiss;	/* input packets missing pcb cache */
				/* output statistics: */
	int	udps_opackets;		/* total output packets */
};

#define	UDP_TTL		30	/* default time to live for UDP packets */

#ifdef KERNEL
struct	inpcb udb;
struct	udpstat udpstat;
#endif
