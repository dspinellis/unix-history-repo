/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Tony Nardo of the Johns Hopkins University/Applied Physics Lab.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)sprint.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/time.h>
#include <tzfile.h>
#include <stdio.h>
#include "finger.h"

extern int entries;

sflag_print()
{
	extern time_t now;
	register PERSON *pn;
	register WHERE *w;
	register int cnt;
	register char *p;
	PERSON **list, **sort();
	time_t time();
	char *ctime();

	list = sort();
	/*
	 * short format --
	 *	login name
	 *	real name
	 *	terminal name (the XX of ttyXX)
	 *	if terminal writeable (add an '*' to the terminal name
	 *		if not)
	 *	if logged in show idle time and day logged in, else
	 *		show last login date and time.  If > 6 moths,
	 *		show year instead of time.
	 *	office location
	 *	office phone
	 */
#define	MAXREALNAME	20
	(void)printf("%-*s %-*s %s\n", UT_NAMESIZE, "Login", MAXREALNAME,
	    "Name", "Tty  Idle  Login        Office      Office Phone");
	for (cnt = 0; cnt < entries; ++cnt) {
		pn = list[cnt];
		for (w = pn->whead; w != NULL; w = w->next) {
			(void)printf("%-*.*s %-*.*s ", UT_NAMESIZE, UT_NAMESIZE,
			    pn->name, MAXREALNAME, MAXREALNAME,
			    pn->realname ? pn->realname : "");
			if (!w->loginat) {
				(void)printf("  *     *  No logins   ");
				goto office;
			}
			(void)putchar(w->info == LOGGEDIN && !w->writable ?
			    '*' : ' ');
			if (*w->tty)
				(void)printf("%-2.2s ",
				    w->tty[0] != 't' || w->tty[1] != 't' ||
				    w->tty[2] != 'y' ? w->tty : w->tty + 3);
			else
				(void)printf("   ");
			if (w->info == LOGGEDIN) {
				stimeprint(w);
				(void)printf("  ");
			} else
				(void)printf("    *  ");
			p = ctime(&w->loginat);
			(void)printf("%.6s", p + 4);
			if (now - w->loginat >= SECSPERDAY * DAYSPERNYEAR / 2)
				(void)printf("  %.4s", p + 20);
			else
				(void)printf(" %.5s", p + 11);
office:			if (pn->office)
				(void)printf(" %-11.11s", pn->office);
			else if (pn->officephone)
				(void)printf(" %-11.11s", " ");
			if (pn->officephone)
				(void)printf(" %s", pn->officephone);
			putchar('\n');
		}
	}
}

PERSON **
sort()
{
	register PERSON *pn, **lp;
	PERSON **list;
	int psort();
	char *malloc();

	if (!(list = (PERSON **)malloc((u_int)(entries * sizeof(PERSON *))))) {
		(void)fprintf(stderr, "finger: out of space.\n");
		exit(1);
	}
	for (lp = list, pn = phead; pn != NULL; pn = pn->next)
		*lp++ = pn;
	(void)qsort(list, entries, sizeof(PERSON *), psort);
	return(list);
}

psort(p, t)
	PERSON **p, **t;
{
	return(strcmp((*p)->name, (*t)->name));
}

stimeprint(w)
	WHERE *w;
{
	register struct tm *delta;

	delta = gmtime(&w->idletime);
	if (!delta->tm_yday)
		if (!delta->tm_hour)
			if (!delta->tm_min)
				(void)printf("     ");
			else
				(void)printf("%5d", delta->tm_min);
		else
			(void)printf("%2d:%02d",
			    delta->tm_hour, delta->tm_min);
	else
		(void)printf("%4dd", delta->tm_yday);
}
