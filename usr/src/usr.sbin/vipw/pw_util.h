/*-
 * Copyright (c) 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pw_util.h	8.2 (Berkeley) %G%
 */

void	pw_edit __P((int));
void	pw_error __P((char *, int, int));
void	pw_init __P((void));
int	pw_lock __P((void));
int	pw_mkdb __P((void));
void	pw_prompt __P((void));
int	pw_tmp __P((void));
