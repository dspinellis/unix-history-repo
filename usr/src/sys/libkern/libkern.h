/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)libkern.h	7.1 (Berkeley) %G%
 */

/* Prototypes for non-quad routines. */

int	 bcmp __P((const void *, const void *, size_t));
int	 ffs __P((int));
int	 locc __P((int mask, char *cp, unsigned size));
int	 scanc __P((unsigned size, u_char *cp, u_char *table, int mask));
int	 skpc __P((int mask, int size, char *cp));
char	*strcat __P((char *, const char *));
char	*strcpy __P((char *, const char *));
size_t	 strlen __P((const char *));
char	*strncpy __P((char *, const char *, size_t));
