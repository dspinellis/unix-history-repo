/*
 * print.c - debugging printout routines
 *
 * Copyright (c) Ian F. Darwin, 1987.
 * Written by Ian F. Darwin.
 *
 * This software is not subject to any license of the American Telephone
 * and Telegraph Company or of the Regents of the University of California.
 *
 * Permission is granted to anyone to use this software for any purpose on
 * any computer system, and to alter it and redistribute it freely, subject
 * to the following restrictions:
 *
 * 1. The author is not responsible for the consequences of use of this
 *    software, no matter how awful, even if they arise from flaws in it.
 *
 * 2. The origin of this software must not be misrepresented, either by
 *    explicit claim or by omission.  Since few users ever read sources,
 *    credits must appear in the documentation.
 *
 * 3. Altered versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.  Since few users
 *    ever read sources, credits must appear in the documentation.
 *
 * 4. This notice may not be removed or altered.
 */

#include <stdio.h>
#include <errno.h>
#include "file.h"

#ifndef	lint
static char *moduleid = 
	"@(#)$Header: print.c,v 1.11 88/01/15 12:17:06 ian Exp $";
#endif	/* lint */

#define MAXSTR		500

extern char *progname;
extern char *magicfile;
extern int debug, nmagic;	/* number of valid magic[]s */
extern void showstr();

mdump(m)
struct magic *m;
{
	(void) printf("%d\t%d\t%d\t%c\t",
		m->contflag,
		m->offset,
		m->type,
		m->reln,
		0);
	if (m->type == STRING)
		showstr(m->value.s);
	else
		(void) printf("%d",m->value.l);
	(void) printf("\t%s", m->desc);
	(void) putchar('\n');
}

/*
 * error - print best error message possible and exit
 */
/*ARGSUSED1*/
/*VARARGS*/
void
error(s1, s2)
char *s1, *s2;
{
	warning(s1, s2);
	exit(1);
}

/*ARGSUSED1*/
/*VARARGS*/
warning(f, a)
char *f, *a;
{
	extern int errno, sys_nerr;
	extern char *sys_errlist[];
	int myerrno;

	myerrno = errno;

	/* cuz we use stdout for most, stderr here */
	(void) fflush(stdout); 

	if (progname != NULL) {
		(void) fputs(progname, stderr);
		(void) putc(':', stderr);
		(void) putc(' ', stderr);
	}
	(void) fprintf(stderr, f, a);
	if (myerrno > 0 && myerrno < sys_nerr)
		(void) fprintf(stderr, " (%s)", sys_errlist[myerrno]);
	putc('\n', stderr);
}
