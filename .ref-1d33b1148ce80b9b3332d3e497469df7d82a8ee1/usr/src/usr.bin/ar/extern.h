/*-
 * Copyright (c) 1991, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	8.3 (Berkeley) %G%
 */

int	append __P((char **));
void	badfmt __P((void));
int	compare __P((char *));
int	contents __P((char **));
int	delete __P((char **));
void	error __P((char *));
int	extract __P((char **));
char   *files __P((char **argv));
int	move __P((char **));
void	orphans __P((char **argv));
int	print __P((char **));
int	replace __P((char **));
char   *rname __P((char *));
int	tmp __P((void));

extern char *archive;
extern char *posarg, *posname;		/* positioning file name */
extern char *tname;                     /* temporary file "name" */
extern CHDR chdr;			/* converted header */
