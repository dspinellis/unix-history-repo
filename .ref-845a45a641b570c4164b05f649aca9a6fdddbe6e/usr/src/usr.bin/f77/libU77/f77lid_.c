/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)f77lid_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * f77lid_ - the ID strings for f77 libraries.
 *
 * Usage:
 *	include 'external f77lid' in the declarations in any f77 module.
 */

extern char	libU77_id[], libI77_id[], libF77_id[];
char	*f77lid_[] = { libU77_id, libI77_id, libF77_id };
