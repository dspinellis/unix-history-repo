/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	5.1 (Berkeley) %G%
 */

#include <sys/cdefs.h>

__BEGIN_DECLS
void	 badfmt __P((void));
void	 error __P((char *));
void	 orphans __P((char **argv));
int	 compare __P((char *));
int	 tmp __P((void));
char	*files __P((char **argv));
char	*rname __P((char *));
__END_DECLS
