/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	5.3 (Berkeley) %G%
 */

typedef struct {
	char *p_end;			/* pointer to NULL at end of path */
	char p_path[MAXPATHLEN + 1];	/* pointer to the start of a path */
} PATH_T;

extern char *progname;			/* program name */

#include <sys/cdefs.h>

__BEGIN_DECLS
void	 err __P((const char *fmt, ...));
int	 path_set __P((PATH_T *, char *));
char	*path_append __P((PATH_T *, char *, int));
char	*path_basename __P((PATH_T *));
void	 path_restore __P((PATH_T *, char *));
__END_DECLS
