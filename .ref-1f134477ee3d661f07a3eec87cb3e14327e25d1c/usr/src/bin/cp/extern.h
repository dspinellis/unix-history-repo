/*-
 * Copyright (c) 1991, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	8.2 (Berkeley) %G%
 */

typedef struct {
	char *p_end;			/* pointer to NULL at end of path */
	char *target_end;               /* pointer to end of target base */
	char p_path[MAXPATHLEN + 1];	/* pointer to the start of a path */
} PATH_T;

extern PATH_T to;
extern uid_t myuid;
extern int iflag, pflag, myumask;

#include <sys/cdefs.h>

__BEGIN_DECLS
int	copy_fifo __P((struct stat *, int));
int	copy_file __P((FTSENT *, int));
int	copy_link __P((FTSENT *, int));
int	copy_special __P((struct stat *, int));
int	setfile __P((struct stat *, int));
void	usage __P((void));
__END_DECLS
