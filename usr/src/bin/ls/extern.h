/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	5.3 (Berkeley) %G%
 */

int acccmp __P((const FTSENT *, const FTSENT *));
int revacccmp __P((const FTSENT *, const FTSENT *));
int modcmp __P((const FTSENT *, const FTSENT *));
int revmodcmp __P((const FTSENT *, const FTSENT *));
int namecmp __P((const FTSENT *, const FTSENT *));
int revnamecmp __P((const FTSENT *, const FTSENT *));
int statcmp __P((const FTSENT *, const FTSENT *));
int revstatcmp __P((const FTSENT *, const FTSENT *));

void err __P((int, const char *, ...));
void prcopy __P((char *, char *, int));
void printcol __P((DISPLAY *));
void printlong __P((DISPLAY *));
void printscol __P((DISPLAY *));
void usage __P((void));
