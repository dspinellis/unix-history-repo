/*
 * f77lid_ - the ID strings for f77 libraries.
 *	@(#)f77lid_.c	1.1
 * Usage:
 *	include 'external f77lid' in the declarations in any f77 module.
 */

extern char	libU77_id[], libI77_id[], libF77_id[];
char	*f77lid_[] = { libU77_id, libI77_id, libF77_id };
