/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	5.1 (Berkeley) %G%
 */

int	 acccmp __P((LS *, LS *));
void	*emalloc __P((u_int));
void	 err __P((const char *, ...));
int	 modcmp __P((LS *, LS *));
int	 namecmp __P((LS *, LS *));
void	 prcopy __P((char *, char *, int));
void	 printcol __P((LS *, int));
void	 printlong __P((LS *, int));
void	 printscol __P((LS *, int));
int	 revacccmp __P((LS *, LS *));
int	 revmodcmp __P((LS *, LS *));
int	 revnamecmp __P((LS *, LS *));
int	 revstatcmp __P((LS *, LS *));
int	 statcmp __P((LS *, LS *));
void	 usage __P((void));
void	 warn __P((const char *, ...));
