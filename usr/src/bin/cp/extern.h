/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	8.1 (Berkeley) %G%
 */

typedef struct {
	char *p_end;			/* pointer to NULL at end of path */
	char *target_end;               /* pointer to end of target base */
	char p_path[MAXPATHLEN + 1];	/* pointer to the start of a path */
} PATH_T;

extern char *progname;			/* program name */
extern PATH_T to;
extern int iflag, pflag;
extern uid_t myuid;
extern int exit_val, myumask;

#include <sys/cdefs.h>

__BEGIN_DECLS
void copy_fifo __P((struct stat *, int));
void copy_file __P((FTSENT *, int));
void copy_link __P((FTSENT *, int));
void copy_special __P((struct stat *, int));
void err __P((const char *fmt, ...));
void setfile __P((struct stat *, int));
void usage __P((void));
__END_DECLS
