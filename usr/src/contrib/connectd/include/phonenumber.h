/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Bill Jolitz.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)phonenumber.h	5.2 (Berkeley) 5/29/93
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
