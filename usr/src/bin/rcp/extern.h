/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	5.1 (Berkeley) %G%
 */

typedef struct {
	int cnt;
	char *buf;
} BUF;

extern int iamremote;

BUF	*allocbuf __P((BUF *, int, int));
char	*colon __P((char *));
void	 err __P((const char *, ...));
void	 lostconn __P((int));
void	 nospace __P((void));
int	 okname __P((char *));
int	 susystem __P((char *, int));
void	 verifydir __P((char *));
