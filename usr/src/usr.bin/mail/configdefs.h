/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)configdefs.h	5.2 (Berkeley) %G%
 */

/*
 * This file contains the definitions of data structures used in
 * configuring the network behavior of Mail when replying.
 */

/*
 * The following constants are used when you are running 4.1a bsd or
 * later on a local network.  Under control of the #define flag
 * GETHOST, the host name is determined dynamically using the
 * gethostname() system call.  The name thus found is inserted
 * into the host table slot whose name was originally EMPTY.
 */
#define	EMPTY		"** empty **"
#define	EMPTYID		'E'

/*
 * The following data structure is the host table.  You must have
 * an entry here for your own machine, plus any special stuff you
 * expect the mailer to know about.  If you have #define'd GETHOST
 * in v7.local.h, you needn't add your machine to the host table.
 * Not all hosts need be here, however:
 * Mail can dope out stuff about hosts on the fly by looking
 * at addresses.  The machines needed here are:
 *	1) The local machine
 *	2) Any machines on the path to a network gateway
 *	3) Any machines with nicknames that you want to have considered
 *	   the same.
 * The machine id letters can be anything you like and are not seen
 * externally.  Be sure not to use characters with the 0200 bit set --
 * these have special meanings.
 */
struct netmach {
	char	*nt_machine;
	char	nt_mid;
	short	nt_type;
};

/*
 * Network type codes.  Basically, there is one for each different
 * network, if the network can be discerned by the separator character,
 * such as @ for the arpa net.  The purpose of these codes is to
 * coalesce cases where more than one character means the same thing,
 * such as % and @ for the arpanet.  Also, the host table uses a
 * bit map of these codes to show what it is connected to.
 * BN -- connected to Bell Net.
 * AN -- connected to ARPA net, SN -- connected to Schmidt net.
 */
#define	AN	1			/* Connected to ARPA net */
#define	BN	2			/* Connected to BTL net */
#define	SN	4			/* Connected to Schmidt net */

/*
 * Data structure for table mapping network characters to network types.
 */
struct ntypetab {
	char	nt_char;		/* Actual character separator */
	int	nt_bcode;		/* Type bit code */
};

/*
 * Codes for the "kind" of a network.  IMPLICIT means that if there are
 * physically several machines on the path, one does not list them in the
 * address.  The arpa net is like this.  EXPLICIT means you list them,
 * as in UUCP.
 * By the way, this distinction means we lose if anyone actually uses the
 * arpa net subhost convention: name@subhost@arpahost
 */
#define	IMPLICIT	1
#define	EXPLICIT	2

/*
 * Table for mapping a network code to its type -- IMPLICIT routing or
 * IMPLICIT routing.
 */
struct nkindtab {
	int	nk_type;		/* Its bit code */
	int	nk_kind;		/* Whether explicit or implicit */
};

/*
 * The following table gives the order of preference of the various
 * networks.  Thus, if we have a choice of how to get somewhere, we
 * take the preferred route.
 */
struct netorder {
	short	no_stat;
	char	no_char;
};

/*
 * External declarations for above defined tables.
 */
#ifndef CONFIGFILE
extern struct netmach netmach[1];
extern struct ntypetab ntypetab[1];
extern struct nkindtab nkindtab[1];
extern struct netorder netorder[1];
extern char *metanet;
#endif
