/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)libkern.h	8.2 (Berkeley) %G%
 */

#include <sys/types.h>

static __inline int
imax(a, b)
	int a, b;
{
	return (a > b ? a : b);
}
static __inline int
imin(a, b)
	int a, b;
{
	return (a < b ? a : b);
}
static __inline long
lmax(a, b)
	long a, b;
{
	return (a > b ? a : b);
}
static __inline long
lmin(a, b)
	long a, b;
{
	return (a < b ? a : b);
}
static __inline u_int
max(a, b)
	u_int a, b;
{
	return (a > b ? a : b);
}
static __inline u_int
min(a, b)
	u_int a, b;
{
	return (a < b ? a : b);
}
static __inline u_long
ulmax(a, b)
	u_long a, b;
{
	return (a > b ? a : b);
}
static __inline u_long
ulmin(a, b)
	u_long a, b;
{
	return (a < b ? a : b);
}

/* Prototypes for non-quad routines. */
int	 bcmp __P((const void *, const void *, size_t));
int	 ffs __P((int));
int	 locc __P((int, char *, u_int));
u_long	 random __P((void));
char	*rindex __P((const char *, int));
int	 scanc __P((u_int, u_char *, u_char *, int));
int	 skpc __P((int, int, char *));
char	*strcat __P((char *, const char *));
char	*strcpy __P((char *, const char *));
size_t	 strlen __P((const char *));
char	*strncpy __P((char *, const char *, size_t));
