/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)string.h	5.1 (Berkeley) 85/08/05
 */

#include <strings.h>

/*
 * these next few are obsolete trash
 */

extern char *strcpyn();
extern char *strcatn();
extern int strcmpn();

/*
 * and the rest are Sys5 functions supported just so
 * Sys5 progs will compile easily.
 */

extern char *strchr();
extern char *strrchr();
extern char *strpbrk();
extern int strspn();
extern int strcspn();
extern char *strtok();
