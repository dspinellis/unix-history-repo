/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mystring.h	5.1 (Berkeley) %G%
 */

#ifndef SYSV
#define strchr mystrchr
#endif

#ifdef __STDC__
void scopyn(const char *, char *, int);
char *strchr(const char *, int);
void mybcopy(const pointer, pointer, int);
int prefix(const char *, const char *);
int number(const char *);
int is_number(const char *);
int strcmp(const char *, const char *);	/* from C library */
char *strcpy(char *, const char *);	/* from C library */
int strlen(const char *);		/* from C library */
char *strcat(char *, const char *);	/* from C library */
#else
void scopyn();
char *strchr();
void mybcopy();
int prefix();
int number();
int is_number();
int strcmp();
char *strcpy();
int strlen();
char *strcat();
#endif

#define equal(s1, s2)	(strcmp(s1, s2) == 0)
#define scopy(s1, s2)	((void)strcpy(s2, s1))
#define bcopy(src, dst, n)	mybcopy((pointer)(src), (pointer)(dst), n)
