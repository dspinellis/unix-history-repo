/*
 * Copyright (c) 1984, 1985, 1986, 1987, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)idp_var.h	8.1 (Berkeley) %G%
 */

/*
 * IDP Kernel Structures and Variables
 */
struct	idpstat {
	int	idps_badsum;		/* checksum bad */
	int	idps_tooshort;		/* packet too short */
	int	idps_toosmall;		/* not enough data */
	int	idps_badhlen;		/* ip header length < data size */
	int	idps_badlen;		/* ip length < ip header length */
};

#ifdef KERNEL
struct	idpstat	idpstat;
#endif
