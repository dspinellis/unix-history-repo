/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Bill Jolitz.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)phonenumber.h	5.2 (Berkeley) %G%
 */

/*
 * Telephone number address processing
 */

/*
 * Structures returned by network
 * data base library.
 */
struct	phonenumberent {
	char	*pn_name;	/* directory listing name */
	char	**pn_aliases;	/* alias list */
	int	pn_length;	/* length of phone number address */
	char	**pn_addr_list;	/* list of addresses from name server */
	struct	pn_addr	*pn_addrtype;	/* list of phone number address types */
};

struct pn_addrtype {
	u_char	pn_tos ;	/* type of service */
# define	PNT_POTS	0	/* plain ordinary telephone service */
# define	PNT_ISDN	1	/* Integrated Services Digital Network*/
	int	pn_tnplan ;	/* type of numbering plan */
# define	PNAT_INTL	0x21	/* International number */
# define	PNAT_NATNL	0x41	/* National number */
# define	PNAT_LOCAL	0x81	/* Local(directory) number */
} ;

struct phonenumberent	*getphonenumberbyname(), *getnamebyphonenumber(),
			*getphonenumberent();

/*
 * Error return codes from getphonenumberbyname() and getnamebyphonenumber()
 */

extern  int pn_errno;	

#define	ENTRY_NOT_FOUND	1 /* Authoritive Answer Host not found */
#define	TRY_AGAIN	2 /* Non-Authoritive Host not found, or SERVERFAIL */
#define	NO_RECOVERY	3 /* Non recoverable errors, FORMERR, REFUSED, NOTIMP */
#define NO_ADDRESS	4 /* Valid host name, no address, look for MX record */
