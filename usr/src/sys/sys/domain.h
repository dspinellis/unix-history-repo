/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)domain.h	7.2 (Berkeley) %G%
 */

/*
 * Structure per communications domain.
 */
struct	domain {
	int	dom_family;		/* AF_xxx */
	char	*dom_name;
	int	(*dom_init)();		/* initialize domain data structures */
	int	(*dom_externalize)();	/* externalize access rights */
	int	(*dom_dispose)();	/* dispose of internalized rights */
	struct	protosw *dom_protosw, *dom_protoswNPROTOSW;
	struct	domain *dom_next;
};

#ifdef KERNEL
struct	domain *domains;
#endif
