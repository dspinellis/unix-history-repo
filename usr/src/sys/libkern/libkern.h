/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)libkern.h	7.3 (Berkeley) %G%
 */

#include <sys/types.h>

/* Prototypes for non-quad routines. */

int	 bcmp __P((const void *, const void *, size_t));
int	 ffs __P((int));
int	 imax __P((int, int));
int	 imin __P((int, int));
long	 lmax __P((long, long));
long	 lmin __P((long, long));
int	 locc __P((int, char *, u_int));
u_int	 max __P((u_int, u_int));
u_int	 min __P((u_int, u_int));
u_long	 random __P((void));
int	 scanc __P((u_int, u_char *, u_char *, int));
int	 skpc __P((int, int, char *));
char	*strcat __P((char *, const char *));
char	*strcpy __P((char *, const char *));
size_t	 strlen __P((const char *));
char	*strncpy __P((char *, const char *, size_t));
u_long	 ulmax __P((u_long, u_long));
u_long	 ulmin __P((u_long, u_long));
