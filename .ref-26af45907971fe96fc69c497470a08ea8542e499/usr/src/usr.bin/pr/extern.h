/*-
 * Copyright (c) 1991 Keith Muller.
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Keith Muller of the University of California, San Diego.
 *
 * %sccs.include.redist.c%
 *
 *      @(#)extern.h	5.2 (Berkeley) %G%
 */

extern int eoptind;
extern char *eoptarg;

void	 addnum __P((char *, int, int));
int	 egetopt __P((int, char * const *, const char *));
void	 flsh_errs __P((void));
int	 horzcol __P((int, char **));
int	 inln __P((FILE *, char *, int, int *, int, int *));
int	 inskip __P((FILE *, int, int));
void	 mfail __P((void));
int	 mulfile __P((int, char **));
FILE	*nxtfile __P((int, char **, char **, char *, int));
int	 onecol __P((int, char **));
int	 otln __P((char *, int, int *, int *, int));
void	 pfail __P((void));
int	 prhead __P((char *, char *, int));
int	 prtail __P((int, int));
int	 setup __P((int, char **));
void	 terminate __P((int));
void	 usage __P((void));
int	 vertcol __P((int, char **));
