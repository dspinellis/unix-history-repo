/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	5.2 (Berkeley) %G%
 */

int	 acccmp __P((const FTSENT *, const FTSENT *));
int	 modcmp __P((const FTSENT *, const FTSENT *));
int	 namecmp __P((const FTSENT *, const FTSENT *));
int	 revacccmp __P((const FTSENT *, const FTSENT *));
int	 revmodcmp __P((const FTSENT *, const FTSENT *));
int	 revnamecmp __P((const FTSENT *, const FTSENT *));
int	 revstatcmp __P((const FTSENT *, const FTSENT *));
int	 statcmp __P((const FTSENT *, const FTSENT *));

void	*emalloc __P((u_int));
void	 err __P((int, const char *, ...));
void	 prcopy __P((char *, char *, int));
void	 printcol __P((FTSENT *, int, u_long, int));
void	 printlong __P((FTSENT *, int, u_long, int));
void	 printscol __P((FTSENT *, int, u_long, int));
void	 usage __P((void));
