/*-
 * Copyright (c) 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pw_util.h	8.1 (Berkeley) %G%
 */

extern void	pw_edit __P((int));
extern void	pw_error __P((char *, int, int));
extern void	pw_init __P((void));
extern int	pw_lock __P((void));
extern int	pw_mkdb __P((void));
extern void	pw_prompt __P((void));
extern int	pw_tmp __P((void));
