/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)f77lid_.c	5.1	%G%
 */

/*
 * f77lid_ - the ID strings for f77 libraries.
 *
 * Usage:
 *	include 'external f77lid' in the declarations in any f77 module.
 */

extern char	libU77_id[], libI77_id[], libF77_id[];
char	*f77lid_[] = { libU77_id, libI77_id, libF77_id };
