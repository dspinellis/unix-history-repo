/*-
 * Copyright (c) 1980, 1989, 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)systat.h	5.10 (Berkeley) %G%
 */

#include <curses.h>

struct  cmdtab {
        char    *c_name;		/* command name */
        void    (*c_refresh)();		/* display refresh */
        void    (*c_fetch)();		/* sets up data structures */
        void    (*c_label)();		/* label display */
	int	(*c_init)();		/* initialize namelist, etc. */
	WINDOW	*(*c_open)();		/* open display */
	void	(*c_close)();		/* close display */
	int	(*c_cmd)();		/* display command interpreter */
	char	c_flags;		/* see below */
};

#define	CF_INIT		0x1		/* been initialized */
#define	CF_LOADAV	0x2		/* display w/ load average */

#define	TCP	0x1
#define	UDP	0x2

#define KREAD(addr, buf, len)  kvm_ckread((addr), (buf), (len))
#define NVAL(indx)  namelist[(indx)].n_value
#define NPTR(indx)  (void *)NVAL((indx))
#define NREAD(indx, buf, len) kvm_ckread(NPTR((indx)), (buf), (len))
#define LONG	(sizeof (long))
