/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lpc.h	5.4 (Berkeley) %G%
 */

/*
 * Line printer control program.
 */
struct	cmd {
	char	*c_name;		/* command name */
	char	*c_help;		/* help message */
	int	(*c_handler)();		/* routine to do the work */
	int	c_priv;			/* privileged command */
};
