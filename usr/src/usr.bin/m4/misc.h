/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)misc.h	5.1 (Berkeley) %G%
 */

#ifdef __STDC__
#define proto(s) s
#else
#define proto(s) ()
#endif

int indx proto((char *s1, char *s2));
void putback proto((int c));
void pbstr proto((char *s));
void pbnum proto((int n));
void chrsave proto((int c));
void getdiv proto((int ind));
void oops proto((char *s1, char *s2));
void onintr proto((void));
void killdiv proto((void));
char *xalloc proto((unsigned long n));
char *strdup proto((char *s));
char *basename proto((char *s));
void usage proto((void));

#ifdef GETOPT
int getopt proto((int argc, char *argv[], char *optstring));
#endif

#ifdef DUFFCP
int memcpy proto((char *to, char *from, int count));
#endif

#undef proto
