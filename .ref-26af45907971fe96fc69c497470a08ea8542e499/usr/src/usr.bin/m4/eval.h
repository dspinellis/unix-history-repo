/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)eval.h	5.1 (Berkeley) %G%
 */

#ifdef __STDC__
#define proto(s) s
#else
#define proto(s) ()
#endif

void eval proto((char *argv[], int argc, int td));
void expand proto((char *argv[], int argc));
void dodefine proto((char *name, char *defn));
void dodefn proto((char *name));
void dopushdef proto((char *name, char *defn));
void dodump proto((char *argv[], int argc));
void doifelse proto((char *argv[], int argc));
int doincl proto((char *ifile));
int dopaste proto((char *pfile));
void dochq proto((char *argv[], int argc));
void dochc proto((char *argv[], int argc));
void dodiv proto((int n));
void doundiv proto((char *argv[], int argc));
void dosub proto((char *argv[], int argc));
void map proto((char *dest, char *src, char *from, char *to));

#undef proto
