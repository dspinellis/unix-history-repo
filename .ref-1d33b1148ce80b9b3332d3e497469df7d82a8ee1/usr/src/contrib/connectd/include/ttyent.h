/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Bill Jolitz.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ttyent.h	5.2 (Berkeley) %G%
 */

struct	ttyent { /* see getttyent(3) */
	char	*ty_name;	/* terminal device name */
	char	*ty_getty;	/* command to execute, usually getty */
	char	*ty_type;	/* terminal type for termcap (3X) */
	int	ty_status;	/* status flags (see below for defines) */
	char 	*ty_window;	/* command to start up window manager */
	char	*ty_comment;	/* usually the location of the terminal */
	char	**ty_flags ;	/* list of flag strings, null terminated */
};

#define TTY_ON		0x1	/* enable logins (startup getty) */
#define TTY_SECURE	0x2	/* allow root to login */

extern struct ttyent *getttyent();
extern struct ttyent *getttynam();
