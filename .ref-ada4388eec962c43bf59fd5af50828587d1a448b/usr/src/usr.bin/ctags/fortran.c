/*
 * Copyright (c) 1987, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)fortran.c	8.3 (Berkeley) %G%";
#endif /* not lint */

#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>

#include "ctags.h"

static void takeprec __P((void));

char *lbp;				/* line buffer pointer */

int
PF_funcs()
{
	bool	pfcnt;			/* pascal/fortran functions found */
	char	*cp;
	char	tok[MAXTOKEN];

	for (pfcnt = NO;;) {
		lineftell = ftell(inf);
		if (!fgets(lbuf, sizeof(lbuf), inf))
			return (pfcnt);
		++lineno;
		lbp = lbuf;
		if (*lbp == '%')	/* Ratfor escape to fortran */
			++lbp;
		for (; isspace(*lbp); ++lbp)
			continue;
		if (!*lbp)
			continue;
		switch (*lbp | ' ') {	/* convert to lower-case */
		case 'c':
			if (cicmp("complex") || cicmp("character"))
				takeprec();
			break;
		case 'd':
			if (cicmp("double")) {
				for (; isspace(*lbp); ++lbp)
					continue;
				if (!*lbp)
					continue;
				if (cicmp("precision"))
					break;
				continue;
			}
			break;
		case 'i':
			if (cicmp("integer"))
				takeprec();
			break;
		case 'l':
			if (cicmp("logical"))
				takeprec();
			break;
		case 'r':
			if (cicmp("real"))
				takeprec();
			break;
		}
		for (; isspace(*lbp); ++lbp)
			continue;
		if (!*lbp)
			continue;
		switch (*lbp | ' ') {
		case 'f':
			if (cicmp("function"))
				break;
			continue;
		case 'p':
			if (cicmp("program") || cicmp("procedure"))
				break;
			continue;
		case 's':
			if (cicmp("subroutine"))
				break;
		default:
			continue;
		}
		for (; isspace(*lbp); ++lbp)
			continue;
		if (!*lbp)
			continue;
		for (cp = lbp + 1; *cp && intoken(*cp); ++cp)
			continue;
		if (cp = lbp + 1)
			continue;
		*cp = EOS;
		(void)strcpy(tok, lbp);
		getline();			/* process line for ex(1) */
		pfnote(tok, lineno);
		pfcnt = YES;
	}
	/*NOTREACHED*/
}

/*
 * cicmp --
 *	do case-independent strcmp
 */
int
cicmp(cp)
	char	*cp;
{
	int	len;
	char	*bp;

	for (len = 0, bp = lbp; *cp && (*cp &~ ' ') == (*bp++ &~ ' ');
	    ++cp, ++len)
		continue;
	if (!*cp) {
		lbp += len;
		return (YES);
	}
	return (NO);
}

static void
takeprec()
{
	for (; isspace(*lbp); ++lbp)
		continue;
	if (*lbp == '*') {
		for (++lbp; isspace(*lbp); ++lbp)
			continue;
		if (!isdigit(*lbp))
			--lbp;			/* force failure */
		else
			while (isdigit(*++lbp))
				continue;
	}
}
