/*-
 * Copyright (c) 1991 Keith Muller.
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Keith Muller of the University of California, San Diego.
 *
 * %sccs.include.redist.c%
 *
 *      @(#)extern.h	5.1 (Berkeley) %G%
 */

/*
 * External references from each source file
 */

#include <sys/cdefs.h>

extern int	eoptind;
extern char	*eoptarg;

/*
 * functions
 */
int onecol __P((int, char **));
int vertcol __P((int, char **));
int horzcol __P((int, char **));
int mulfile __P((int, char **));
int inln __P((FILE *, char *, register int, int *, int, int *));
int otln __P((register char *, int, int *, int *, int));
int inskip __P((FILE *, register int, register int));
void addnum __P((register char *, register int, register int));
int prhead __P((char *, char *, int));
int prtail __P((register int, int));
void terminate __P((int));
void flsh_errs __P((void));
void mfail __P((void));
void pfail __P((void));
void usage __P((void));
FILE * nxtfile __P((int, char **, char **, char *, int));
int setup __P((register int, register char **));
int egetopt __P((int, char * const *, const char *));
